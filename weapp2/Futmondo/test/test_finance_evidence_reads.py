"""Synthetic immutable-state evidence reuse; never uses live accounts."""
import copy
import json
import unittest
import httpx
from worker.main import Store, WorkerFailure

FIELDS='account_id,championship_id,competition_id,season,evidence_type,source_id,observed_at,payload'
PARAMS=dict(account_id='eq.a',championship_id='eq.l',competition_id='eq.c',season='eq.s',select=FIELDS,limit=1000,order='source_id.asc')

class EvidenceReads(unittest.IsolatedAsyncioTestCase):
    async def asyncSetUp(self):
        self.rows=[dict(account_id='a',championship_id='l',competition_id='c',season='s',evidence_type='team',source_id='t',observed_at='2026-01-01',state_id=1)]
        self.payloads={1:{'history':'x'*100000},2:{'history':'changed'}}
        self.calls=[];self.fail=False;self.omit=False;self.wire=[]
        async def reply(request):
            path=request.url.path.split('/')[-1];self.calls.append((path,dict(request.url.params)))
            if self.fail:return httpx.Response(500,json={'code':'57014'})
            if path=='rival_finance_evidence_versions':value=copy.deepcopy(self.rows)
            elif path=='finance_history_states':
                ids=[int(x) for x in request.url.params['state_id'][4:-1].split(',')]
                value=[] if self.omit else [dict(state_id=i,payload=self.payloads[i]) for i in ids]
            else:value=[]
            self.wire.append(len(json.dumps(value)));return httpx.Response(200,json=value)
        self.client=httpx.AsyncClient(transport=httpx.MockTransport(reply));self.store=Store(self.client,'https://example.invalid','synthetic')
    async def asyncTearDown(self):await self.client.aclose()
    async def read(self,**overrides):return await self.store.request('GET','rival_finance_evidence_latest',params=dict(PARAMS,**overrides))
    async def test_repeated_payload_once_fresh_timestamp_and_exact_result(self):
        first=await self.read();cold=sum(self.wire);self.wire=[]
        self.rows[0]['observed_at']='2026-02-01'
        second=await self.read();warm=sum(self.wire)
        self.assertEqual(second[0]['observed_at'],'2026-02-01')
        self.assertEqual(second[0]['payload'],first[0]['payload']);self.assertNotIn('state_id',second[0])
        self.assertEqual([p for p,_ in self.calls].count('finance_history_states'),1)
        self.assertLess(warm,cold/100)
        # A consumer cannot mutate the cached authoritative payload.
        second[0]['payload']['history']='tampered';self.assertEqual((await self.read())[0]['payload'],self.payloads[1])
        print('MEASURE evidence response JSON bytes cold=%d warm=%d'%(cold,warm))
    async def test_correction_return_and_deletion(self):
        await self.read();self.rows[0]['state_id']=2
        self.assertEqual((await self.read())[0]['payload'],self.payloads[2])
        self.rows[0]['state_id']=1
        self.assertEqual((await self.read())[0]['payload'],self.payloads[1])
        self.assertEqual([p for p,_ in self.calls].count('finance_history_states'),2)
        self.rows=[];self.assertEqual(await self.read(),[])
    async def test_failure_and_incomplete_never_use_stale_page(self):
        await self.read();self.fail=True
        with self.assertRaises(WorkerFailure):await self.read()
        self.fail=False;self.rows[0]['state_id']=2;self.omit=True
        with self.assertRaisesRegex(WorkerFailure,'database_evidence_incomplete'):await self.read()
    async def test_scope_and_projection_fallback(self):
        await self.read();self.rows[0]['account_id']='b'
        await self.read(account_id='eq.b')
        self.assertEqual([p for p,_ in self.calls].count('finance_history_states'),2)
        with self.assertRaisesRegex(WorkerFailure,'database_evidence_shape'):await self.read()
        await self.read(select='payload,observed_at')
        self.assertEqual(self.calls[-1][0],'rival_finance_evidence_latest')
        await self.read(**{'or':'(season.eq.unknown,competition_id.eq.unknown)'})
        self.assertEqual(self.calls[-1][0],'rival_finance_evidence_latest')
    async def test_page_filter_preserved_and_only_new_state_downloaded(self):
        await self.read();row=dict(self.rows[0],state_id=2,source_id='t2');self.rows.append(row)
        await self.read(offset=5)
        metadata=[q for p,q in self.calls if p=='rival_finance_evidence_versions'][-1]
        self.assertEqual(metadata['offset'],'5');self.assertEqual(metadata['order'],'source_id.asc')
        self.assertEqual([q for p,q in self.calls if p=='finance_history_states'][-1]['state_id'],'in.(2)')
    async def test_memory_bound(self):
        for i in range(20):
            self.rows[0]['state_id']=i+1;self.payloads[i+1]={'data':'z'*1000000}
            await self.read()
        self.assertLessEqual(self.store.evidence_bytes,16*1024*1024)
        self.assertNotIn(('a',1),self.store.evidence_payloads)

if __name__=='__main__':unittest.main()
