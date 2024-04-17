import re

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
    handle("c-a")(get_by_name("beginning-of-line"))
    handle("c-b")(get_by_name("backward-char"))
    # c-e c-f breaks auto suggestion
    handle("c-e")(get_by_name("end-of-line"))
    # handle("c-f")(get_by_name("forward-char"))
    handle("c-left")(get_by_name("backward-word"))
    handle("c-right")(get_by_name("forward-word"))

    handle("escape", "d", filter=vi_insert_mode)(get_by_name("kill-word"))

    handle("escape", "b")(get_by_name("backward-word"))
    handle("escape", "f")(get_by_name("forward-word"))

    @handle("c-n", filter=vi_insert_mode)
    def _next(event) -> None:
        "Next line."
        event.current_buffer.auto_down()

    @handle("c-p", filter=vi_insert_mode)
    def _prev(event) -> None:
        "Previous line."
        event.current_buffer.auto_up(count=event.arg)

    @handle(Keys.ControlV)
    def _paste(event):
        raw = event.app.clipboard.get_data()
        text: str = raw.text
        if re.match(r"^\s*(http|ftp|magnet)", text):
            text = text.strip()
            text = f'"{text}"'
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

    handle('escape', 'escape')(handle_prefix('sudo'))
    handle('c-x', 'c-p')(handle_prefix('proxychains -q'))
