"""Conditional DB snapshot reads: synthetic transport, no live credentials."""
import json
import unittest
import httpx
from worker.main import Store, pack_snapshot, stamp, WorkerFailure


def snapshot(value, version='v1'):
    chunks,size,digest=pack_snapshot(value)
    return dict(key='key',manifest=dict(version=version,parts=len(chunks),json_bytes=size,sha256=digest,
        valid_until=stamp(),context={'account_id':'a'},status='ok'),chunks=chunks)


class EgressTests(unittest.IsolatedAsyncioTestCase):
    async def test_unchanged_payload_downloaded_once_metadata_stays_current(self):
        item=snapshot({'rows':list(range(1000))})
        requests=[];bodies=[]
        async def reply(request):
            payload=json.loads(request.content);requests.append(payload)
            if payload['p_versions']['key']=='v1':
                manifest=dict(item['manifest'],status='partial',valid_until='2000-01-01T00:00:00+00:00')
                result=dict(resources=[],unchanged=[dict(key='key',manifest=manifest)])
            else: result=dict(resources=[item],unchanged=[])
            bodies.append(len(json.dumps(result)))
            return httpx.Response(200,json=result)
        async with httpx.AsyncClient(transport=httpx.MockTransport(reply)) as client:
            store=Store(client,'https://example.invalid','synthetic')
            first=await store.rpc('read',p_account='a',p_versions={'key':''})
            again=await store.rpc('read',p_account='a',p_versions={'key':''})
            self.assertEqual(first['resources'][0]['chunks'],again['resources'][0]['chunks'])
            self.assertEqual(again['resources'][0]['manifest']['status'],'partial')
            self.assertEqual(requests[1]['p_versions']['key'],'v1')
            self.assertLess(bodies[1],bodies[0]/2)
            print('Synthetic repeated read bytes:',bodies)

    async def test_changed_missing_and_account_isolation(self):
        current=snapshot({'value':1});requests=[]
        async def reply(request):
            requests.append(json.loads(request.content))
            return httpx.Response(200,json=dict(resources=[current] if current else [],unchanged=[]))
        async with httpx.AsyncClient(transport=httpx.MockTransport(reply)) as client:
            store=Store(client,'https://example.invalid','synthetic')
            await store.rpc('read',p_account='a',p_versions={'key':''})
            await store.rpc('read',p_account='b',p_versions={'key':''})
            self.assertEqual(requests[-1]['p_versions']['key'],'')
            current=snapshot({'value':2},'v2')
            result=await store.rpc('read',p_account='a',p_versions={'key':''})
            self.assertEqual(result['resources'][0]['manifest']['version'],'v2')
            current=None
            self.assertEqual((await store.rpc('read',p_account='a',p_versions={'key':''}))['resources'],[])
            self.assertNotIn(('a','key'),store.snapshot_cache)

    async def test_failures_do_not_fall_back_to_memory(self):
        item=snapshot({'ok':True});fail=False
        async def reply(request):
            return httpx.Response(500,json={'code':'57014'}) if fail else httpx.Response(200,json=dict(resources=[item]))
        async with httpx.AsyncClient(transport=httpx.MockTransport(reply)) as client:
            store=Store(client,'https://example.invalid','synthetic')
            await store.rpc('read',p_account='a',p_versions={'key':''})
            fail=True
            with self.assertRaisesRegex(WorkerFailure,'database_500'):
                await store.rpc('read',p_account='a',p_versions={'key':''})

    async def test_corrupt_chunks_are_not_cached(self):
        item=snapshot({'ok':True});item['chunks']=['bad']
        async def reply(request):return httpx.Response(200,json=dict(resources=[item]))
        async with httpx.AsyncClient(transport=httpx.MockTransport(reply)) as client:
            store=Store(client,'https://example.invalid','synthetic')
            with self.assertRaises(Exception):await store.rpc('read',p_account='a',p_versions={'key':''})
            self.assertEqual(store.snapshot_cache_bytes,0)
            self.assertFalse(store.snapshot_cache)

    async def test_traffic_reports_counts_and_bytes_without_payloads(self):
        async def reply(request):return httpx.Response(200,json={'private':'SECRET'})
        async with httpx.AsyncClient(transport=httpx.MockTransport(reply)) as client:
            store=Store(client,'https://example.invalid','SECRET_KEY')
            await store.request('GET','worker_jobs')
            await store.request('GET','worker_jobs')
            with self.assertLogs('futmondo.worker',level='INFO') as logs:store.report_traffic()
            self.assertIn('endpoint=worker_jobs calls=2',logs.output[0])
            self.assertNotIn('SECRET',logs.output[0])
            self.assertEqual(store.traffic,{})

    async def test_entry_bound_and_explicit_versions(self):
        async def reply(request):
            body=json.loads(request.content);key=next(iter(body['p_versions']))
            item=snapshot({'ok':True});item['key']=key
            return httpx.Response(200,json=dict(resources=[item]))
        async with httpx.AsyncClient(transport=httpx.MockTransport(reply)) as client:
            store=Store(client,'https://example.invalid','synthetic')
            for i in range(130):await store.rpc('read',p_account='a',p_versions={str(i):''})
            self.assertEqual(len(store.snapshot_cache),128)
            self.assertLess(store.snapshot_cache_bytes,8*1024*1024)
            self.assertNotIn(('a','0'),store.snapshot_cache)

if __name__=='__main__':unittest.main()
