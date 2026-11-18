import asyncio
import unittest
from types import SimpleNamespace
from unittest.mock import AsyncMock
from worker.main import Worker

class CleanupTests(unittest.IsolatedAsyncioTestCase):
    async def test_failure_does_not_clear_leadership_and_retries(self):
        state=SimpleNamespace(leader=True, store=SimpleNamespace(rpc=AsyncMock(side_effect=[RuntimeError(),{'chunks':0,'checkpoints':0}])))
        await Worker.cleanup_once(state)
        self.assertTrue(state.leader)
        await Worker.cleanup_once(state)
        self.assertEqual(state.store.rpc.await_count,2)
    async def test_standby_does_not_clean(self):
        state=SimpleNamespace(leader=False,store=SimpleNamespace(rpc=AsyncMock()))
        await Worker.cleanup_once(state)
        state.store.rpc.assert_not_called()
    async def test_slow_cleanup_yields(self):
        entered=asyncio.Event();release=asyncio.Event()
        async def delayed(*args,**kwargs):
            entered.set();await release.wait();return {'chunks':0,'checkpoints':0}
        state=SimpleNamespace(leader=True,store=SimpleNamespace(rpc=delayed))
        task=asyncio.create_task(Worker.cleanup_once(state))
        await entered.wait();await asyncio.sleep(0)
        self.assertFalse(task.done());release.set();await task
if __name__=='__main__':unittest.main()
