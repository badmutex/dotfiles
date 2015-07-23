#!/usr/bin/env python

import hashlib
import os.path
import os
import urllib


def get_hexdigest(data, method='sha256'):
    if method == 'sha256':
        hasher = hashlib.sha256()
    else:
        raise ValueError('Unsupport hashing method {}'.format(method))

    hasher.update(data)
    return hasher.hexdigest()


def same_hash(data0, data1, method='sha256'):
    hex0 = get_hexdigest(data0, method=method)
    hex1 = get_hexdigest(data1, method=method)
    return hex0 == hex1


def write_file(path, contents):
    with open(path, 'w') as fd:
        fd.write(contents)


def main():

    module = AnsibleModule(
        argument_spec = {
            'url': {'required': True},
            'prefix': {'required': False,
                       'default': '/etc/yum.repos.d'},
            'sha256': {'required': False,
                       'default': None},
            
            }
        )

    url = module.params['url']
    prefix = module.params['prefix']
    sha256 = module.params['sha256']

    basename = os.path.basename(url)
    dest = os.path.join(prefix, basename)

    url_contents = urllib.urlopen(url).read()

    if not os.path.exists(dest):
        write_file(dest, url_contents)
        module.exit_json(changed=True)

    else:
        local_contents = open(dest).read()

        if same_hash(local_contents, url_contents):
            module.exit_json(changed=False)
        else:
            write_repo(dest, url_contents)
            module.exit_json(changed=True)

    
from ansible.module_utils.basic import *
if __name__ == '__main__':
    main()
