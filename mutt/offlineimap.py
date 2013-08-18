import os
import subprocess

def mailpasswd(acct):
    path = "~/.mutt/.%s.gpg" % acct
    cmd = "gpg --use-agent --quiet --batch -d %s" % path
    proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, shell=True)
    output = proc.communicate()[0].strip()
    retcode = proc.wait()
    if retcode == 0:
        return output
    else:
        return ''
