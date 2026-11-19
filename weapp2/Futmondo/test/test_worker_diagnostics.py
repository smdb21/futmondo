"""Safe worker diagnostics; synthetic transport only."""
import asyncio
import os
import unittest
from pathlib import Path
from unittest.mock import patch

import httpx
from worker.main import Store, Worker, WorkerFailure, source_rejection_detail


class DiagnosticTests(unittest.IsolatedAsyncioTestCase):
    async def test_database_failure_keeps_code_and_redacts_payload(self):
        async def reply(request):
            return httpx.Response(500, json={'code': '57014', 'message': 'SECRET SQL',
                                            'details': 'SECRET ROW', 'hint': 'SECRET TOKEN'})
        async with httpx.AsyncClient(transport=httpx.MockTransport(reply)) as client:
            store = Store(client, 'https://example.invalid', 'SECRET KEY')
            with self.assertLogs('futmondo.worker', level='WARNING') as logs:
                for _ in range(2):
                    with self.assertRaisesRegex(WorkerFailure, '^database_500$'):
                        await store.request('POST', 'rpc/worker_write_fact', json={'token':'SECRET'})
            self.assertEqual(len(logs.output), 1)
            self.assertIn('endpoint=rpc/worker_write_fact status=500 detail=57014', logs.output[0])
            self.assertNotIn('SECRET', logs.output[0])

    async def test_quota_restriction_is_distinct_from_database_failure(self):
        async def reply(request):
            return httpx.Response(402, json={'message':'SECRET quota response'})
        async with httpx.AsyncClient(transport=httpx.MockTransport(reply)) as client:
            with self.assertLogs('futmondo.worker',level='WARNING') as logs:
                with self.assertRaisesRegex(WorkerFailure,'^database_402$'):
                    await Store(client,'https://example.invalid','SECRET').request('GET','worker_jobs')
            self.assertIn('status=402', logs.output[0])
            self.assertNotIn('SECRET', logs.output[0])

    async def test_transport_failure_and_unknown_path_are_redacted(self):
        async def reply(request):
            raise httpx.ReadTimeout('SECRET URL AND KEY', request=request)
        async with httpx.AsyncClient(transport=httpx.MockTransport(reply)) as client:
            store = Store(client, 'https://example.invalid', 'SECRET')
            with self.assertLogs('futmondo.worker', level='WARNING') as logs:
                with self.assertRaisesRegex(WorkerFailure, '^database_connection$'):
                    await store.request('GET', 'private_SECRET?token=SECRET')
            self.assertIn('endpoint=other status=connection detail=read_timeout', logs.output[0])
            self.assertNotIn('SECRET', logs.output[0])

    async def test_untrusted_database_code_and_non_json(self):
        for payload in ({'code':'SECRET\nINJECTION'}, ['SECRET'], None):
            async def reply(request):
                return httpx.Response(502, json=payload) if payload is not None else httpx.Response(502, text='SECRET')
            async with httpx.AsyncClient(transport=httpx.MockTransport(reply)) as client:
                with self.assertLogs('futmondo.worker', level='WARNING') as logs:
                    with self.assertRaisesRegex(WorkerFailure, '^database_502$'):
                        await Store(client, 'https://example.invalid', 'SECRET').request('GET','worker_jobs')
                self.assertIn('detail=unknown', logs.output[0])
                self.assertNotIn('SECRET', logs.output[0])

    async def test_success_emits_no_diagnostic_or_extra_request(self):
        calls = []
        async def reply(request):
            calls.append(request)
            return httpx.Response(200, json=[{'ok':True}])
        async with httpx.AsyncClient(transport=httpx.MockTransport(reply)) as client:
            with self.assertNoLogs('futmondo.worker', level='WARNING'):
                self.assertEqual(await Store(client,'https://example.invalid','key').request('GET','worker_jobs'),[{'ok':True}])
        self.assertEqual(len(calls), 1)

    async def test_configuration_reports_effective_custom_and_default(self):
        for value, expected, source in [('45',45,'environment'), ('',30,'default')]:
            with patch.dict(os.environ, {'FUTMONDO_WORKER_BASE_INTERVAL_MINUTES':value}):
                worker = Worker(None, None, 'ab'*32, Path.cwd())
                worker.stop.set()
                with self.assertLogs('futmondo.worker', level='INFO') as logs:
                    await worker.run()
                self.assertIn(f'base_interval_minutes={expected} interval_source={source}',logs.output[0])

    async def test_maintenance_logs_policy_once_without_extra_calls(self):
        class FakeStore:
            calls = 0
            async def rpc(self, name, **kwargs):
                self.calls += 1
                self.params = kwargs
                worker.stop.set()
                return {'leader':True,'admitted':True}
        with patch.dict(os.environ, {'FUTMONDO_WORKER_BASE_INTERVAL_MINUTES':'45'}):
            store = FakeStore()
            worker = Worker(store,None,'ab'*32,Path.cwd())
            with self.assertLogs('futmondo.worker',level='INFO') as logs:
                await worker.maintenance()
            self.assertEqual(store.calls,1)
            self.assertEqual(store.params['p_base_interval'],2700)
            self.assertIn('leader=True admitted=True base_interval_minutes=45',logs.output[0])

    def test_source_rejections_correlate_teams_and_parent_jobs_without_ids(self):
        worker=Worker(None,None,'ab'*32,Path.cwd())
        job=dict(account_id='PRIVATE_ACCOUNT',resource_key='PRIVATE_JOB',run_id='PRIVATE_RUN',attempts=3,
                 descriptor=dict(handler='get_user_team_info',context={'user_team_id':'PRIVATE_TEAM'},
                                 args={'user_team_id':'PRIVATE_TEAM'}))
        body=dict(header={'token':'PRIVATE_TOKEN'},query={'championshipId':'PRIVATE_LEAGUE','userteamId':'PRIVATE_TEAM'})
        rejection={'answer':{'error':True,'code':'api.error.invalid','message':'PRIVATE_MESSAGE'}}
        with self.assertLogs('futmondo.worker',level='WARNING') as logs:
            worker.log_source_rejection(job,'owner','/1/userteam/information',body,rejection,200)
            job['descriptor']['handler']='fm_worker_finances';job['resource_key']='PRIVATE_PARENT'
            worker.log_source_rejection(job,'owner','/1/userteam/information',body,rejection,200)
            body['query']['userteamId']='PRIVATE_OTHER'
            worker.log_source_rejection(job,'owner','/1/userteam/information',body,rejection,200)
        self.assertNotIn('PRIVATE',str(logs.output))
        self.assertIn('detail=api.error.invalid error_flag=True',logs.output[0])
        self.assertIn('process_count=2',logs.output[1])
        self.assertIn('handler=fm_worker_finances',logs.output[1])
        self.assertIn('team_context=no team_argument=no',logs.output[2])
        def fields(line):return dict(x.split('=',1) for x in line.split() if '=' in x)
        a,b,c=map(fields,logs.output)
        self.assertEqual(a['team_ref'],b['team_ref']);self.assertEqual(a['request_ref'],b['request_ref'])
        self.assertNotEqual(a['job_ref'],b['job_ref']);self.assertNotEqual(a['team_ref'],c['team_ref'])
        again=Worker(None,None,'ab'*32,Path.cwd())
        self.assertEqual(worker.source_reference('team','a','l','t'),again.source_reference('team','a','l','t'))
        self.assertNotEqual(worker.source_reference('team','a','l','t'),worker.source_reference('team','b','l','t'))

    def test_source_diagnostic_memory_is_bounded(self):
        worker=Worker(None,None,'ab'*32,Path.cwd())
        job=dict(account_id='a',resource_key='k',descriptor={'handler':'get_user_team_info'})
        with patch('worker.main.LOG.warning'):
            for i in range(520):
                worker.log_source_rejection(job,'owner','/1/userteam/information',
                    {'query':{'userteamId':str(i)}},{'error':True},200)
        self.assertEqual(len(worker.source_diagnostics),512)

    def test_source_detail_is_allowlisted(self):
        self.assertEqual(source_rejection_detail({'answer':{'error':'api.error.general'}}),'api.error.general')
        for code in ('api.error.invalid','api.userteam.access_denied','api.error.general'):
            self.assertEqual(source_rejection_detail({'answer':{'error':True,'code':code}}),code)
        for body in ({'answer':'api.error.SECRET'}, {'answer':{'code':'api.error.bad\nprivate'}},
                     {'answer':{'code':'api.error.'+'x'*121}}, {'answer':{'code':'api.error.507f191e810c19729de860ea'}}, {'answer':{'error':True,'message':'SECRET'}},
                     {'code':{'private':'SECRET'}}, {'answer':{'code':['SECRET']}}):
            self.assertEqual(source_rejection_detail(body),'unclassified')


if __name__ == '__main__':
    unittest.main()
