"""Validate the exact Docker build marker without building/deploying an image."""
import hashlib
import json
from pathlib import Path
import shlex
import subprocess
import tempfile

root = Path.cwd()
lines = root.joinpath('Dockerfile').read_text().splitlines()
command = next(line.removeprefix('RUN ') for line in lines if line.startswith('RUN python3 -c ') and 'player-card-runtime.json' in line)
expected = ('Modules/Selected_Player_Module.R', 'Modules/Players_in_Championship_Module.R', 'Modules/Market_Module.R', 'Modules/Today_Module.R', 'player_availability.R')
with tempfile.TemporaryDirectory(prefix='futmondo-runtime-') as directory:
    target = Path(directory)
    target.joinpath('Modules').mkdir()
    target.joinpath('www').mkdir()
    for name in expected:
        target.joinpath(name).write_bytes(root.joinpath(name).read_bytes())
    subprocess.run(shlex.split(command), cwd=target, check=True)
    result = json.loads(target.joinpath('www/player-card-runtime.json').read_text())
    assert result['version'] == 1
    assert result['behavior'] == 'target-only-selection'
    assert set(result['sha256']) == set(expected)
    for name in expected:
        assert result['sha256'][name] == hashlib.sha256(root.joinpath(name).read_bytes()).hexdigest()
print('PASS Docker runtime marker contains only the five exact public code fingerprints; no account/environment data')
