import re

from xonsh import platform
from prompt_toolkit.clipboard import ClipboardData
from prompt_toolkit.filters.app import vi_insert_mode
from prompt_toolkit.key_binding.bindings.named_commands import get_by_name
from prompt_toolkit.keys import Keys
from prompt_toolkit.selection import PasteMode

$VI_MODE = True

@events.on_ptk_create
def custom_keybindings(bindings, **kw):
    handle = bindings.add

    # Emacs
    handle("c-a", filter=vi_insert_mode)(get_by_name("beginning-of-line"))
    handle("c-b", filter=vi_insert_mode)(get_by_name("backward-char"))
    # c-e c-f breaks auto suggestion
    handle("c-e", filter=vi_insert_mode)(get_by_name("end-of-line"))
    # handle("c-f")(get_by_name("forward-char"))
    handle("c-left", filter=vi_insert_mode)(get_by_name("backward-word"))
    handle("c-right", filter=vi_insert_mode)(get_by_name("forward-word"))

    handle("escape", "d", filter=vi_insert_mode)(get_by_name("kill-word"))

    handle("escape", "b", filter=vi_insert_mode)(get_by_name("backward-word"))
    handle("escape", "f", filter=vi_insert_mode)(get_by_name("forward-word"))

    handle("escape", "backspace", filter=vi_insert_mode)(get_by_name("backward-kill-word"))

    @handle("c-n", filter=vi_insert_mode)
    def _next(event) -> None:
        "Next line."
        event.current_buffer.auto_down()

    @handle("c-p", filter=vi_insert_mode)
    def _prev(event) -> None:
        "Previous line."
        event.current_buffer.auto_up(count=event.arg)

    @handle("c-v", filter=vi_insert_mode)
    def _paste(event):
        raw = event.app.clipboard.get_data()
        text: str = raw.text
        # remove CR
        text = text.replace('\r', '')
        if re.match(r"^\s*(https?://|ftp://|magnet:?|C:\\)", text):
            text = text.strip()
            text = f'r"{text}"'
        d = ClipboardData(text, raw.type)
        event.current_buffer.paste_clipboard_data(
            d, count=event.arg, paste_mode=PasteMode.EMACS
        )

    def handle_prefix(prefix):
        def handler(event):
            text = event.current_buffer.text
            if text.strip() == '':
                event.current_buffer.auto_up()
                text = event.current_buffer.text
            if not text.strip().startswith(prefix):
                event.current_buffer.transform_current_line(lambda x: f'{prefix} {x}')
                event.current_buffer.cursor_position += len(prefix) + 1
        return handler

    handle('escape', 'escape', filter=vi_insert_mode)(handle_prefix('sudo'))
    handle('c-x', 'c-p', filter=vi_insert_mode)(handle_prefix('proxychains -q'))

