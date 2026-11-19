"""Synthetic worker activity telemetry; no source or database network calls."""
import asyncio
import copy
import json
import unittest
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import AsyncMock, patch

import httpx

from worker.main import Worker, WorkerFailure
from test_worker import FakeStore, KEY, descriptor


class ActivityTests(unittest.IsolatedAsyncioTestCase):
    async def asyncSetUp(self):
        self.store = FakeStore()
        self.requests = []

        async def transport(request):
            self.requests.append(request)
            return httpx.Response(200, json={'answer': []})

        self.client = httpx.AsyncClient(transport=httpx.MockTransport(transport))
        self.worker = Worker(self.store, self.client, KEY, Path.cwd())
        self.worker.admission_blocked = False
        desc = copy.deepcopy(descriptor())
        self.job = dict(account_id='fixture', resource_key=desc['resource_key'], generation=1,
                        interval_seconds=300, descriptor=desc, attempts=1,
                        run_id='11111111-1111-4111-8111-111111111111')
        self.owner = self.worker.identity + ':0'

    async def asyncTearDown(self):
        await self.client.aclose()
        await asyncio.gather(*(helper.close() for helper in self.worker.helpers))

    def message(self):
        return dict(type='http', url='https://api.futmondo.com/1/league/championshipteams',
                    body={'query': {'championshipId': 'l'}, 'answer': []})

    async def test_existing_guard_carries_only_safe_activity_and_same_checkpoint(self):
        for route in ('https://private.invalid/?token=PRIVATE', '/1/player/summary?token=PRIVATE',
                      'remembered_logins', {'password': 'PRIVATE'}, ['PRIVATE']):
            self.worker.set_activity(self.owner, 'database', route, 'POST')
            await self.worker.guard(self.job, self.owner, checkpoint={'cursor': 3})
            name, params = self.store.calls[-1]
            self.assertEqual(name, 'guard_activity')
            self.assertEqual(params['p_checkpoint'], {'cursor': 3})
            activity = params['p_activity']
            self.assertIsNone(activity['route'])
            self.assertIsNone(activity['method'])
            self.assertEqual(activity['run_id'], self.job['run_id'])
            self.assertEqual(set(activity), {'phase', 'route', 'method', 'started_at', 'sequence', 'run_id'})
            self.assertNotIn('PRIVATE', json.dumps(activity))
        sequences = [params['p_activity']['sequence'] for _, params in self.store.calls]
        self.assertEqual(sequences, sorted(set(sequences)))
        self.assertEqual(len(self.store.calls), 5)  # No separate telemetry writes.
        self.worker.set_activity(self.owner, 'PRIVATE', '/1/player/summary', 'PRIVATE')
        self.assertEqual(self.worker.activities[self.owner]['phase'], 'processing')
        self.assertIsNone(self.worker.activities[self.owner]['method'])
        self.store.allowed = False
        with self.assertRaisesRegex(WorkerFailure, 'access_revoked'):
            await self.worker.guard(self.job, self.owner)

    async def test_source_activity_uses_existing_dispatch_guard_and_no_extra_requests(self):
        self.worker.set_activity(self.owner, 'processing')
        await self.worker.exchange(self.job, self.owner, self.message())
        self.assertEqual([name for name, _ in self.store.calls],
                         ['prepare_source', 'guard_activity', 'checkpoint_ack'])
        sample = self.store.calls[1][1]['p_activity']
        self.assertEqual(sample['phase'], 'source_request')
        self.assertEqual(sample['method'], 'POST')
        self.assertEqual(sample['route'], '/1/league/championshipteams')
        self.assertEqual(len(self.requests), 1)
        self.assertNotIn('synthetic', json.dumps(sample))
        self.assertNotIn('query', sample)
        self.assertEqual(self.worker.activities[self.owner]['phase'], 'processing')
        self.assertIsNone(self.worker.activities[self.owner]['route'])

    async def test_delayed_source_is_sampled_by_existing_lane_heartbeat(self):
        entered, release, sampled = asyncio.Event(), asyncio.Event(), asyncio.Event()
        original_rpc = self.store.rpc
        source_samples = 0

        async def rpc(name, **params):
            nonlocal source_samples
            if name == 'claim':
                return copy.deepcopy(self.job)
            if name == 'guard_activity' and params['p_activity']['phase'] == 'source_request':
                source_samples += 1
                if source_samples >= 2:
                    sampled.set()
            return await original_rpc(name, **params)

        async def transport(request):
            entered.set()
            await release.wait()
            return httpx.Response(200, json={'answer': []})

        async def run(desc, exchange):
            await exchange(self.message())
            self.worker.stop.set()
            return {'value': {'kind': 'null'}, 'status': 'ok'}

        original_sleep = asyncio.sleep

        async def accelerated_heartbeat(delay):
            # Exercise the production renewal loop without a fifteen-second test.
            await original_sleep(.01 if delay == 15 else delay)

        self.store.rpc = rpc
        self.client._transport = httpx.MockTransport(transport)
        self.worker.helpers[0] = SimpleNamespace(run=run, close=AsyncMock())
        self.worker.leader = True
        with patch('worker.main.asyncio.sleep', accelerated_heartbeat):
            task = asyncio.create_task(self.worker.lane(0))
            try:
                await asyncio.wait_for(entered.wait(), 1)
                await asyncio.wait_for(sampled.wait(), 1)
                self.assertFalse(task.done())
                samples = [p['p_activity'] for name, p in self.store.calls if name == 'guard_activity']
                self.assertEqual(samples[-1]['route'], '/1/league/championshipteams')
                release.set()
                await asyncio.wait_for(task, 1)
            finally:
                release.set()
                if not task.done():
                    task.cancel()
                    await asyncio.gather(task, return_exceptions=True)
        self.assertEqual(self.worker.activities, {})
        self.assertEqual(set(self.worker.activity_sequences), {self.owner})

    async def test_database_read_reports_allowlisted_table_without_query(self):
        self.worker.set_activity(self.owner, 'processing')
        await self.worker.exchange(self.job, self.owner,
            dict(type='database', table='player_history', query={'private': 'PRIVATE'}))
        names = [name for name, _ in self.store.calls]
        self.assertEqual(names, ['guard_activity', 'player_history'])
        sample = self.store.calls[0][1]['p_activity']
        self.assertEqual((sample['phase'], sample['route'], sample['method']),
                         ('database', 'player_history', 'GET'))
        self.assertNotIn('PRIVATE', json.dumps(sample))
        self.assertEqual(self.worker.activities[self.owner]['phase'], 'processing')

    async def test_failure_and_cancellation_clear_transient_request_activity(self):
        with self.assertRaisesRegex(WorkerFailure, 'source_route_denied'):
            await self.worker.source(self.job, self.owner,
                dict(self.message(), url='https://api.futmondo.com/1/player/summary?token=PRIVATE'))
        self.assertEqual(self.worker.activities, {})
        self.assertEqual(self.store.calls, [])
        entered = asyncio.Event()

        async def transport(request):
            entered.set()
            await asyncio.Event().wait()

        self.client._transport = httpx.MockTransport(transport)
        task = asyncio.create_task(self.worker.source(self.job, self.owner, self.message()))
        await asyncio.wait_for(entered.wait(), 1)
        task.cancel()
        with self.assertRaises(asyncio.CancelledError):
            await task
        self.assertEqual(self.worker.activities, {})

    async def test_cancelled_lane_discards_activity_without_leaking_slots(self):
        entered = asyncio.Event()
        original_rpc = self.store.rpc

        async def rpc(name, **params):
            return copy.deepcopy(self.job) if name == 'claim' else await original_rpc(name, **params)

        async def run(*args):
            entered.set()
            await asyncio.Event().wait()

        self.store.rpc = rpc
        self.worker.helpers[0] = SimpleNamespace(run=run, close=AsyncMock())
        self.worker.leader = True
        task = asyncio.create_task(self.worker.lane(0))
        await asyncio.wait_for(entered.wait(), 1)
        self.assertEqual(len(self.worker.activities), 1)
        task.cancel()
        with self.assertRaises(asyncio.CancelledError):
            await task
        self.assertEqual(self.worker.activities, {})
        self.worker.helpers[0].close.assert_awaited_once()


if __name__ == '__main__':
    unittest.main()
