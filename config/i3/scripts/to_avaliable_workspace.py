import subprocess
import json

is_empty = len(
    subprocess.Popen(['i3-save-tree'], stdout=subprocess.PIPE)
    .stdout.read()) > 20

if (is_empty):
    workspaces = json.loads(
        subprocess.Popen(
            ['i3-msg', '-t', 'get_workspaces'], stdout=subprocess.PIPE)
        .stdout.read())

    avaliable_workspace = (next(
        filter(
            lambda x: x not in [workspace['num'] for workspace in workspaces],
            range(1, 9))))

    subprocess.call(['i3-msg', 'workspace', str(avaliable_workspace)])
    with open('/home/ben/i3msg', 'wb') as f:
        f.write(
            subprocess.Popen(['i3-msg','-t','get_tree'], stdout=subprocess.PIPE)
            .stdout.read())
else:
    subprocess.call(['i3-msg', 'workspace', 'back_and_forth'])
