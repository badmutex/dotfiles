#!/usr/bin/env python

from collections import namedtuple
import shlex
import subprocess
import re

SSHD_CONFIG_PATH = '/etc/sshd/sshd_config'

location_info = namedtuple('location_info', ('location', 'string'))


class sshd_config(object):
    @classmethod
    def load(cls, path='/etc/ssh/sshd_config'):

        cfg = cls(path=path)

        with open(path) as fd:
            for i, line in enumerate(fd):
                cfg._add_line(i, line)

        return cfg

    def __init__(self, path=None):
        self._lines = []
        self._lookup = {}
        self._changes = 0
        self._path = path or SSHD_CONFIG_PATH

    def _add_line(self, lineno, line):
        self._lines.append(line)

        bare = line.strip()

        if bare and not bare.startswith('#'):
            key, values = re.split('\s+', bare, 1)
            self._lookup[key] = location_info(location=lineno, string=values)

    def _option_known(self, option, values):
        return option in self._lookup and self._lookup[option].string == values

    def _option_set(self, option, values):
        self._changes += 1

        line = '{option} {values}\n'.format(option=option, values=values)

        if option in self._lookup:
            prev = self._lookup[option]
            self._lines[prev.location] = line
            self._lookup[option] = location_info(prev.location, values)
        else:
            self._lookup[option] = location_info(location=len(self._lines),
                                                 string=values)
            self._lines.append(line)

    def set(self, option, values):
        if self._option_known(option, values):
            return False
        else:
            self._option_set(option, values)

    @property
    def changed(self):
        return self._changes > 0


    def save(self, path=None):
        path = path or self._path

        with open(path, 'w') as fd:
            fd.write(''.join(self._lines))


def main():

    module = AnsibleModule(
        argument_spec = {
            'option': {'required': True},
            'value': {'required': True},
            'path': {'default': '/etc/ssh/sshd_config'},
            'validate': {'required': False},
        },
    )

    option = module.params['option']
    value  = module.params['value']
    path   = module.params['path']

    cfg = sshd_config.load(path)
    cfg.set(option, value)

    if module.params['validate']:
        base_cmd = shlex.split(module.params['validate'])

        tmpfile = tempfile.NamedTemporaryFile()
        cfg.save(path=tmpfile.name)
        tmpfile.seek(0)

        cmd = base_cmd + [tmpfile.name]

        try:
            subprocess.check_output(cmd)
        except subprocess.CalledProcessError, e:
            module.fail_json(msg='Validation failed with: {}'.format(e.output))

    if cfg.changed:
        cfg.save()

    module.exit_json(changed=cfg.changed)


from ansible.module_utils.basic import *
if __name__ == '__main__':
    main()


