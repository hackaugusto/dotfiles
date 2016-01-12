#!/usr/bin/env python
# -*- coding: utf-8 -*-
# vim: set sts=4 sw=4 ts=4 et:
import codecs
import sys

import re2


def sed(regex, files, quiet=False, null_separated=False):
    files = files or [sys.stdin]
    for filename in files:
        with codecs.open(filename, codec='utf8') as handler:
            for line in handler:
                pass


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()

    parser.add_argument('-n', '--quiet', '--silent', 'quiet')
    parser.add_argument('-z', '--null-data', dest='null_separated')
    parser.add_argument('-e', '--expression', action='append', dest='regexes')

    # i didn't found a easy way to say "either there is a -e or an positional
    # regex", so let's do it by hand
    # parser.add_argument('files', nargs='+')

    arguments, remaining = parser.parse_known_args()
    mapping = dict(arguments._get_kwargs())

    if mapping['regexes'] is None:
        if not len(remaining):
            parser.print_help()
            sys.exit(1)

        mapping['regexes'] = remaining.pop(0)

    try:
        sed(**dict(arguments))
    except:
        sys.exit(1)
