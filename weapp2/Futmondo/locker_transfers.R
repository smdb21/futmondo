# Locker clause payment and later pressroom delivery are one economic event.
locker_clause_rows <- function(news) {
  out<-list()
  for(item in news){
    if(!identical(fm_scalar(item$styp,""),"clause"))next
    document<-xml2::read_html(paste0('<html><body>',fm_scalar(item$txt,""),'</body></html>'),options=c('RECOVER','NOERROR','NOWARNING','NONET'))
    values<-trimws(xml2::xml_text(xml2::xml_find_all(document,'.//strong')))
    plain<-trimws(xml2::xml_text(xml2::xml_find_first(document,'.//body')))
    if(length(values)!=4L||!grepl('^El equipo .* ha pagado .* propiedad de .* como cl[aá]usula de ',plain)||
       !grepl('^[0-9]{1,3}([.][0-9]{3})*$|^[0-9]+$',values[2]))stop('Locker clause payment format could not be verified')
    amount<-as.numeric(gsub('.','',values[2],fixed=TRUE));id<-fm_scalar(item$`_id`,"");stamp<-fm_scalar(item$created,"")
    if(!nzchar(id)||!is.finite(amount)||amount<=0||is.na(fm_time(stamp))||any(!nzchar(values[c(1,3,4)])))stop('Incomplete locker clause payment')
    out[[length(out)+1L]]<-data.frame(id=paste0('locker:',id),locker_event_id=id,created=stamp,player_name=values[4],buyer_team_name=values[1],seller_team_name=values[3],price=amount,is_clause=TRUE,stringsAsFactors=FALSE)
  }
  rows<-dplyr::bind_rows(out)
  if(nrow(rows))rows<-rows[!duplicated(rows$id,fromLast=TRUE),,drop=FALSE]
  rows
}
get_locker_clause_payments <- function(login,championship_id) {
  get_cached_data(paste0('pressroom_locker_payments_',championship_id),{
    cursor<-'';seen<-character();pages<-list()
    repeat {
      payload<-list(header=list(token=login[['token']],userid=login[['userid']]),query=list(championshipId=championship_id,from=cursor),answer=list())
      response<-futmondo_post('https://api.futmondo.com/2/locker/news',body=jsonlite::toJSON(payload,auto_unbox=TRUE),httr::add_headers(`Content-Type`='application/json; charset=utf-8'))
      answer<-httr::content(response)$answer
      if(!is.list(answer)||!'news'%in%names(answer))stop('Locker news response unavailable')
      news<-answer$news
      if(!length(news))break
      pages[[length(pages)+1L]]<-locker_clause_rows(news)
      next_cursor<-fm_scalar(tail(news,1L)[[1]]$`_id`,'')
      if(!nzchar(next_cursor)||next_cursor%in%c(cursor,seen))stop('Locker news cursor repeated before completion')
      seen<-c(seen,cursor);cursor<-next_cursor
    }
    rows<-dplyr::bind_rows(pages)
    if(nrow(rows))rows<-rows[!duplicated(rows$id,fromLast=TRUE),,drop=FALSE]
    observed_data(rows,status='ok')
  },timeout_sec=300,context=resource_context(login=login,championship_id=championship_id))
}
locker_resolve_parties <- function(rows,pressroom,teams,players=data.frame()) {
  if(!nrow(rows))return(rows)
  dictionary<-function(id,name){d<-unique(data.frame(id=as.character(id),name=trimws(as.character(name))));d[!is.na(d$id)&nzchar(d$id)&!is.na(d$name)&nzchar(d$name),,drop=FALSE]}
  team_names<-dictionary(teams$teamid,teams$teamname)
  if(nrow(pressroom))team_names<-unique(dplyr::bind_rows(team_names,dictionary(pressroom$buyer_team_id,pressroom$buyer_team_name),dictionary(pressroom$seller_team_id,pressroom$seller_team_name)))
  resolve<-function(names,aliases) vapply(trimws(names),function(name){ids<-unique(aliases$id[aliases$name%in%name]);if(length(ids)==1L)ids else ''},character(1))
  rows$buyer_team_id<-resolve(rows$buyer_team_name,team_names);rows$seller_team_id<-resolve(rows$seller_team_name,team_names)
  player_names<-dictionary(character(),character())
  if(nrow(pressroom))player_names<-dictionary(pressroom$player_id,pressroom$player_name)
  if(nrow(players))player_names<-unique(dplyr::bind_rows(player_names,dictionary(players$id,players$name)))
  rows$player_id<-resolve(rows$player_name,player_names)
  # Club at payment time: use the latest earlier source observation, then catalog.
  rows$player_team_name<-rows$player_team_slug<-''
  source_times<-if(nrow(pressroom))fm_time(pressroom$created)else as.POSIXct(character())
  payment_times<-fm_time(rows$created)
  for(i in seq_len(nrow(rows))){
    candidates<-if(nrow(pressroom))which(pressroom$player_id%in%rows$player_id[i]&source_times<=payment_times[i])else integer()
    if(length(candidates)){
      j<-candidates[which.max(source_times[candidates])]
      rows$player_team_name[i]<-fm_scalar(pressroom$player_team_name[j],'');rows$player_team_slug[i]<-fm_scalar(pressroom$player_team_slug[j],'')
    }else if(nrow(players)){
      j<-match(rows$player_id[i],players$id)
      rows$player_team_name[i]<-fm_scalar(players$team[j],'');rows$player_team_slug[i]<-fm_scalar(players$logo[j],'')
    }
  }
  rows
}
# Match identities, parties and amount across the delivery delay. Never use a
# one-second payment/arrival comparison or collapse two ambiguous purchases.
merge_locker_transfers <- function(pressroom,locker) {
  if(!is.data.frame(locker)||!nrow(locker))return(pressroom)
  if(any(!nzchar(locker$buyer_team_id)|!nzchar(locker$seller_team_id)|!nzchar(locker$player_id)))stop('Locker transfer identities require source recovery')
  if(!is.data.frame(pressroom)||!nrow(pressroom))return(locker)
  key<-function(x)paste(x$buyer_team_id,x$seller_team_id,x$player_id,x$price,sep='|')
  pk<-key(pressroom);lk<-key(locker);append<-logical(nrow(locker))
  if(!'locker_event_id'%in%names(pressroom))pressroom$locker_event_id<-''
  if(!'delivery_at'%in%names(pressroom))pressroom$delivery_at<-''
  if(!'is_clause'%in%names(pressroom))pressroom$is_clause<-NA
  for(i in seq_len(nrow(locker))){
    index<-which(pressroom$id%in%locker$id[i] | pressroom$locker_event_id%in%locker$locker_event_id[i])
    if(!length(index)){
      candidates<-which(pk%in%lk[i])
      # Multiple indistinguishable purchases must be investigated, not counted twice.
      if(length(candidates)>1L||length(candidates)&&sum(lk==lk[i])>1L)stop('Ambiguous locker/pressroom transfer overlap')
      index<-candidates
    }
    if(length(index)>1L)stop('Duplicate locker source identity')
    if(length(index)){
      j<-index[1]
      if(!nzchar(fm_scalar(pressroom$delivery_at[j],''))&&pressroom$created[j]!=locker$created[i])pressroom$delivery_at[j]<-pressroom$created[j]
      pressroom$created[j]<-locker$created[i];pressroom$is_clause[j]<-TRUE;pressroom$locker_event_id[j]<-locker$locker_event_id[i]
    }else append[i]<-TRUE
  }
  result<-dplyr::bind_rows(pressroom,locker[append,,drop=FALSE])
  result<-result[!duplicated(result$id,fromLast=TRUE),,drop=FALSE]
  result[order(fm_time(result$created),decreasing=TRUE),,drop=FALSE]
}

locker_saved_transfer_rows <- function(evidence) {
  dplyr::bind_rows(lapply(rival_evidence_rows(evidence,'notifications'),function(row){
    if(!identical(row$payload$kind,'locker_transfer')||!is.list(row$payload$transfer))return(NULL)
    transfer<-row$payload$transfer
    fields<-c('id','locker_event_id','created','delivery_at','player_id','player_name','player_team_name','player_team_slug','buyer_team_id','buyer_team_name','seller_team_id','seller_team_name')
    values<-as.list(setNames(vapply(fields,function(key)fm_scalar(transfer[[key]],''),character(1)),fields))
    values$price<-fm_number(transfer$price);values$is_clause<-isTRUE(transfer$is_clause)
    as.data.frame(values,stringsAsFactors=FALSE)
  }))
}
