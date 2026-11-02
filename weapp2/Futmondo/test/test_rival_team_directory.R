options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
checks<-0L
check<-function(name,expr){force(expr);checks<<-checks+1L;cat('PASS',name,'\n')}
http_calls<-0L
# Captured numeric/date shape, synthetic identities only. No saved authentication.
fixture<-list(answer=list(teams=list(
 list(id='member-a',teamid='team-a',teamname='Alpha',name='A',points=856,teamValue=563697490,lastAccess='2026-11-02T13:33:53.080Z',awards=list(list(id='award'))),
 list(id='member-b',teamid='team-b',teamname='Bravo',name='B',points=791,teamValue=420706889,lastAccess='2026-11-02T07:08:46.858Z'),
 list(id='member-c',teamid='team-c',teamname='Charlie',name='C',points=771,teamValue=381429930,lastAccess='2026-11-02T15:15:53.514Z'),
 list(id='member-d',teamid='team-d',teamname='Zero',points=0,teamValue=0,lastAccess=NULL),
 list(id='member-e',teamid='team-e',teamname='Negative',points=-2),
 list(id='member-f',teamid='team-f',teamname='Unknown'))))
futmondo_post<-function(url,body,...){
 stopifnot(identical(url,TEAMS_URL),jsonlite::fromJSON(body)$query$championshipId%in%c('fixture-league','other-league'))
 http_calls<<-http_calls+1L
 structure(list(status_code=200L,headers=list('Content-Type'='application/json; charset=utf-8'),content=charToRaw(jsonlite::toJSON(fixture,auto_unbox=TRUE,null='null')),url=url),class='response')
}
for(method in c('GET','POST','PUT','PATCH','DELETE'))suppressMessages(trace(method,where=asNamespace('httr'),print=FALSE,tracer=quote(stop('Live HTTP forbidden'))))
auth<-c(token='synthetic',userid='fixture-user',user_name='Fixture')
clear_api_cache()
check('teams response preserves public values and reuses account/league cache',{
 teams<-get_teams(auth,'fixture-league');again<-get_teams(auth,'fixture-league')
 stopifnot(http_calls==1L,identical(teams,again),as.numeric(teams$teamValue[1])==563697490,as.numeric(teams$points[1])==856,teams$lastAccess[1]=='2026-11-02T13:33:53.080Z')
 get_teams(auth,'other-league');other<-auth;other['userid']<-'other-user';get_teams(other,'fixture-league');stopifnot(http_calls==3L)
})
now<-as.POSIXct('2026-11-02 16:00:00',tz='UTC')
evidence<-lapply(seq_len(3),function(i)rival_finance_evidence('fixture-user','fixture-league','unknown','team',teams$teamid[i],
 list(budget=100,teamValue=if(i==1)NA_real_ else 1,points=if(i==1)NA_real_ else 1,observed_at=rival_finance_stamp(now)),now))
check('financial evidence cannot erase or replace authoritative directory values',{
 rows<-build_rival_cash_dataset(teams,'fixture-league',evidence,account_id='fixture-user',now=now)$team_finances
 stopifnot(identical(rows$team_value[1:4],c(563697490,420706889,381429930,0)),identical(rows$points,c(856,791,771,0,-2,NA_real_)),
   identical(rows$last_access[1:3],teams$lastAccess[1:3]),all(is.na(rows$team_value[5:6])))
})
check('initial and error directories preserve public fields and unknown values',{
 directory<-rival_finance_directory(teams);reused<-rival_finance_directory(rows)
 stopifnot(identical(directory$team_value,rows$team_value),identical(directory$last_access,rows$last_access),identical(reused$last_access,rows$last_access),identical(reused$team_value,rows$team_value))
})
check('points pills use background-to-red with distinct signed zero and missing scores',{
 pills<-rivals_points_pills(c(-2,0,2,NA_real_));html<-vapply(pills,as.character,character(1))
 stopifnot(grepl('--fm-metric-bg:#0B120D',html[1],fixed=TRUE),grepl('--fm-metric-bg:#FF6B6B',html[3],fixed=TRUE),grepl('>0</span>',html[2],fixed=TRUE),grepl('Unavailable',html[4]),!grepl('fm-metric-badge',html[4]))
 stopifnot(all(vapply(rivals_points_pills(c(5,5)),function(x)grepl('#0B120D',as.character(x),fixed=TRUE),logical(1))),length(rivals_points_pills(numeric()))==0L)
})
check('both full and fallback widgets keep public columns numeric and selectable',{
 for(data in list(rows,teams)){
  widget<-rivals_finance_table_widget(list(team_finances=data));attrs<-widget$x$tag$attribs
  stopifnot(inherits(widget,'reactable'),identical(attrs$onClick,'select'),identical(attrs$selection,'single'))
  columns<-vapply(attrs$columns,function(x)x$id,character(1));stopifnot(all(c('Points','Last Access','Squad Value')%in%columns))
 }
 stamp<-as.numeric(fm_time('2026-11-02T13:33:53.080Z'))
 stopifnot(grepl('02/11/2026 13:33',as.character(rivals_last_access_cell(stamp)),fixed=TRUE),identical(rivals_last_access_cell(NA_real_),'Unavailable'))
})
check('selected rival valuation matches the table and never invents zero',{
 stopifnot(rivals_summary_values(list(teamValue=1),teams,rows,'team-a')$squad_value==563697490,
   rivals_summary_values(list(teamValue=1),teams,rows,'team-d')$squad_value==0,
   is.na(rivals_summary_values(list(),teams,rows,'team-f')$squad_value))
})
check('directory rendering adds no data requests',stopifnot(http_calls==3L))
for(method in c('GET','POST','PUT','PATCH','DELETE'))suppressMessages(untrace(method,where=asNamespace('httr')))
cat('RIVAL TEAM DIRECTORY:',checks,'passed / 0 failed\n')
if('--browser'%in%commandArgs(TRUE)){
 target<-tempfile('futmondo-rival-admin-');dir.create(target)
 config<-list(budget=300000000,playerMoveInDays=4,enableCoach='automatic',loans=TRUE,marketTimes=1,playerRetention=0,enablingClause=10,extraPay=0,rtv='false',vmb=-1,mtype='normal',ac=TRUE,mnmp=.5,dspct=1,ccr=3,ctt=FALSE,mcpw=-1,rcp=-1,cpt=FALSE,blc=TRUE,apm=TRUE,aes=TRUE,dbp=TRUE,mbp=0,ddt=TRUE,acl=TRUE,avmb=TRUE,mbmv=TRUE,mpm=0)
 admin_rows<-admin_league_settings(info=list(configuration=config),championship_id='fixture-league')
 page<-shiny::fluidPage(tags$head(tags$style(HTML(fm_theme_css())),shiny::includeCSS('www/custom_style.css')),
  tags$h2('Rivals'),rivals_finance_table_widget(list(team_finances=rows)),tags$h2('League settings'),admin_league_settings_UI(admin_rows))
 htmltools::save_html(page,file=file.path(target,'index.html'),libdir='lib')
 status<-system2('node',c('test/test_rival_admin_browser.cjs',shQuote(file.path(target,'index.html'))))
 stopifnot(status==0L)
 unlink(target,recursive=TRUE)
}
