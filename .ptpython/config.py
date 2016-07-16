# -*- coding: utf8 -*-
from __future__ import unicode_literals

from prompt_toolkit.keys import Keys
from pygments.token import Token
from ptpython.layout import CompletionVisualisation

__all__ = (
    'configure',
)


def configure(repl):
    repl.confirm_exit = False
    repl.true_color = True

    repl.show_signature = False
    repl.show_docstring = False
    repl.show_meta_enter_message = False

    repl.completion_visualisation = CompletionVisualisation.MULTI_COLUMN  # (NONE, POP_UP, MULTI_COLUMN or TOOLBAR)
    repl.completion_menu_scroll_offset = 0  # POP_UP scroll_offset in the completion menu.
    repl.show_line_numbers = True
    repl.show_status_bar = True

    repl.show_sidebar_help = False
    repl.highlight_matching_parenthesis = True

    # Line wrapping. (Instead of horizontal scrolling.)
    repl.wrap_lines = True
    repl.enable_mouse_support = False
    repl.complete_while_typing = False
    repl.vi_mode = True
    repl.paste_mode = False
    repl.prompt_style = 'classic'  # 'classic' or 'ipython'
    repl.insert_blank_line_after_output = False

    # History Search.
    # When True, going back in history will filter the history on the records
    # starting with the current input. (Like readline.)
    # Note: When enable, please disable the `complete_while_typing` option.
    #       otherwise, when there is a completion available, the arrows will
    #       browse through the available completions instead of the history.
    repl.enable_history_search = False
    repl.enable_auto_suggest = False
    repl.enable_open_in_editor = True
    repl.enable_system_bindings = True
    repl.enable_input_validation = True

    # >>> list(ptpython.style.get_all_styles())
    # [
    #     'manni', 'igor', 'lovelace', 'xcode', 'vim', 'autumn', 'vs', 'rrt',
    #     'native', 'perldoc', 'borland', 'tango', 'emacs', 'friendly',
    #     'monokai', 'paraiso-dark', 'colorful', 'murphy', 'bw', 'pastie',
    #     'algol_nu', 'paraiso-light', 'trac', 'default', 'algol', 'fruity'
    # ]
    repl.use_code_colorscheme('vim')

    # Install custom colorscheme named 'my-colorscheme' and use it.
    """
    repl.install_ui_colorscheme('my-colorscheme', _custom_ui_colorscheme)
    repl.use_ui_colorscheme('my-colorscheme')
    """

    # Add custom key binding for PDB.
    @repl.add_key_binding(Keys.ControlB)
    def _(event):
        ' Pressing Control-B will insert "pdb.set_trace()" '
        event.cli.current_buffer.insert_text('\nimport pdb; pdb.set_trace()\n')

    # Typing ControlE twice should also execute the current command.
    # (Alternative for Meta-Enter.)
    @repl.add_key_binding(Keys.ControlE, Keys.ControlE)
    def _(event):
        b = event.current_buffer
        if b.accept_action.is_returnable:
            b.accept_action.validate_and_handle(event.cli, b)

    """
    # Custom key binding for some simple autocorrection while typing.
    corrections = {
        'impotr': 'import',
        'pritn': 'print',
    }

    @repl.add_key_binding(' ')
    def _(event):
        ' When a space is pressed. Check & correct word before cursor. '
        b = event.cli.current_buffer
        w = b.document.get_word_before_cursor()

        if w is not None:
            if w in corrections:
                b.delete_before_cursor(count=len(w))
                b.insert_text(corrections[w])

        b.insert_text(' ')
    """
