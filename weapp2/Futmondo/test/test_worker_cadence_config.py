import os
import unittest
from unittest.mock import patch
from worker.main import base_interval_seconds

class CadenceConfig(unittest.TestCase):
    def test_default_and_custom(self):
        for value, expected in [('',1800), ('  ',1800), ('5',300), ('120',7200)]:
            with patch.dict(os.environ, {'FUTMONDO_WORKER_BASE_INTERVAL_MINUTES':value}):
                self.assertEqual(base_interval_seconds(),expected)
        with patch.dict(os.environ, {}, clear=True):
            self.assertEqual(base_interval_seconds(),1800)
    def test_invalid(self):
        for value in ['0','-1','1.5','abc','35791395']:
            with patch.dict(os.environ, {'FUTMONDO_WORKER_BASE_INTERVAL_MINUTES':value}):
                with self.assertRaisesRegex(ValueError,'FUTMONDO_WORKER_BASE_INTERVAL_MINUTES'):
                    base_interval_seconds()

if __name__=='__main__': unittest.main()
