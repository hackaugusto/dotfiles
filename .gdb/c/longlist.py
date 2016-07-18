#!/usr/bin/python
# vim:ft=python ts=4 sw=4 sts=4:
import os
import os.path
import sys

# we don't control the version that gdb is using
# if 'VIRTUAL_ENV' in os.environ:
#     version_info = sys.version_info
#     version = '{}.{}'.format(version_info.major, version_info.minor)
#     lib_path = os.path.join(
#         os.environ['VIRTUAL_ENV'],
#         'lib',
#         'python{}'.format(version)
#     )
#     sys.path.append(lib_path)


from pycparser import parse_file, c_parser, c_generator
import pygments.lexers
import pygments.formatters

formatter = pygments.formatters.Terminal256Formatter()  # (style=...)


class LongList(gdb.Command):
    def __init__(self):
        super(LongList, self).__init__('longlist', gdb.COMMAND_NONE)

    def invoke(self, argument, from_tty):
        try:
            sal = gdb.selected_frame().find_sal()
        except gdb.error:
            gdb.write('No frame is currently selected\n')
            return

        filename = sal.symtab.fullname()
        linenum = sal.line

        if filename is None:
            gdb.write('Unknow current file\n')
            return

        function = gdb.newest_frame().function().name

        # gdb.write('{}:{}:{}'.format(filename, function, linenum))
        if os.path.exists(filename) and filename.endswith('.c'):
            with open(filename) as code_handler:
                code = code_handler.read()

            parser = c_parser.CParser()
            ast = parser.parse(code)
            for ext in ast.ext:
                if ext.decl.name == function:
                    start_line, end_line = ext.coord.line, ext.body.block_items[-1].coord.line
            lexer = pygments.lexers.get_lexer_for_filename(filename)
            source = pygments.highlight(source, lexer, formatter)
            gdb.write(''.join(source.splitlines()[start_line:end_line]))

        else:
            gdb.write('Unknow file {}\n'.format(filename))

LongList()
