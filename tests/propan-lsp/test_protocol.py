#!/usr/bin/env python3
"""End-to-end LSP checks. Run after zig-0.16.0 build install."""
import json
from pathlib import Path
import signal
import subprocess
import tempfile
import sys

ROOT = Path(__file__).resolve().parents[2]
URI = (ROOT / 'tests/propan/sema/lsp-metadata.propan').as_uri()
SOURCE = (ROOT / 'tests/propan/sema/lsp-metadata.propan').read_text()


class Client:
    def __init__(self, encoding='utf-16'):
        self.stderr = tempfile.TemporaryFile()
        self.process = subprocess.Popen(
            [str(ROOT / 'zig-out/bin/propan-lsp')], stdin=subprocess.PIPE,
            stdout=subprocess.PIPE, stderr=self.stderr)
        self.sequence = 0
        result = self.request('initialize', {
            'processId': None, 'rootUri': None,
            'capabilities': {'general': {'positionEncodings': [encoding]}}})
        assert result['capabilities']['positionEncoding'] == encoding
        assert result['capabilities']['inlayHintProvider']['resolveProvider'] is False
        self.notify('initialized', {})
        self.open(SOURCE)

    def send(self, method, params, request_id=None):
        message = {'jsonrpc': '2.0', 'method': method, 'params': params}
        if request_id is not None:
            message['id'] = request_id
        data = json.dumps(message).encode()
        self.process.stdin.write(f'Content-Length: {len(data)}\r\n\r\n'.encode() + data)
        self.process.stdin.flush()

    def request(self, method, params):
        self.sequence += 1
        self.send(method, params, self.sequence)
        headers = {}
        while True:
            line = self.process.stdout.readline()
            assert line, f'server exited: {self.process.poll()}'
            if line == b'\r\n':
                break
            key, value = line.decode().split(':', 1)
            headers[key.lower()] = value.strip()
        response = json.loads(self.process.stdout.read(int(headers['content-length'])))
        assert response['id'] == self.sequence, response
        assert 'error' not in response, response
        return response['result']

    def notify(self, method, params):
        self.send(method, params)

    def open(self, text):
        self.notify('textDocument/didOpen', {'textDocument': {
            'uri': URI, 'languageId': 'propan', 'version': 1, 'text': text}})

    def change(self, text):
        self.notify('textDocument/didChange', {'textDocument': {'uri': URI, 'version': 2},
                                             'contentChanges': [{'text': text}]})

    def at(self, method, line, character):
        return self.request('textDocument/' + method, {
            'textDocument': {'uri': URI}, 'position': {'line': line, 'character': character}})

    def document(self, method, **extra):
        return self.request('textDocument/' + method, {'textDocument': {'uri': URI}, **extra})

    def complete(self, prefix):
        self.change(SOURCE + prefix)
        lines = (SOURCE + prefix).split('\n')
        items = self.at('completion', len(lines) - 1, len(lines[-1]))
        return {item['label']: item for item in items}

    def close(self):
        self.request('shutdown', None)
        self.notify('exit', None)
        self.process.stdin.close()
        assert self.process.wait(timeout=5) == 0
        self.stderr.seek(0)
        log = self.stderr.read().decode()
        self.stderr.close()
        assert 'debug(' not in log and 'debug:' not in log, log[:1000]
        assert 'WriteFailed' not in log and 'panic:' not in log, log[:1000]


def check_inlay_hints(client, text, addresses):
    line_count = text.count('\n') + 1
    hints = client.document('inlayHint', range={
        'start': {'line': 0, 'character': 0},
        'end': {'line': line_count, 'character': 0}})
    assert [(hint['position'], hint['label']) for hint in hints] == [
        ({'line': line, 'character': 0}, addresses.get(line, '       |     | '))
        for line in range(line_count)
    ], hints
    assert all(len(hint['label']) == 15 for hint in hints), hints
    return hints


def run():
    c = Client()
    try:
        directive_samples = {
            '.cogexec': '', '.lutexec': '', '.hubexec': '', '.regspace': '',
            '.data': '', '.org': '4', '.align': '4', '.pack': 'byte',
            '.pic': 'prefer', '.assert': '1', '.fit': '512',
            '.import': '"missing.propan"', '.if': '1', '.elif': '1',
            '.else': '', '.endif': '', 'BYTE': '1', 'WORD': '1',
            'LONG': '1', 'RES': '1', 'FILE': '"missing.bin"',
            'const': 'answer = 42', 'var': 'slot: LONG 0',
        }
        for name, argument in directive_samples.items():
            c.change(f'{name} {argument}\n')
            hover = c.at('hover', 0, 1)
            assert hover is not None, name
            assert name in hover['contents']['value'], hover
            assert 'documentation is coming soon' not in hover['contents']['value'], hover
            assert hover['range'] == {'start': {'line': 0, 'character': 0},
                                      'end': {'line': 0, 'character': len(name)}}, hover
        c.change('.COGEXEC\nlong 1\n')
        assert 'cog RAM' in c.at('hover', 0, 1)['contents']['value']
        assert '32-bit data' in c.at('hover', 1, 1)['contents']['value']
        # Exercise intrinsic metadata across analysis, hover, and completion calls.
        intrinsics = (ROOT / 'tests/propan/sema/lsp-intrinsics.propan').read_text()
        c.change(intrinsics)
        names = ('aug', 'nrel', 'hubaddr', 'cogaddr', 'lutaddr',
                 'localaddr', 'byteoffset', 'wordoffset')
        for _ in range(3):
            lenses = c.document('codeLens')
            assert any('Hub: 0x4' in lens['command']['title'] for lens in lenses)
            for name in names:
                line, text = next((i, line) for i, line in enumerate(intrinsics.splitlines())
                                  if name + '(' in line)
                column = text.index(name + '(')
                hover = c.at('hover', line, column)
                param_name = 'value' if name == 'aug' else 'addr'
                param_type = 'int' if name in ('aug', 'nrel') else 'address'
                assert f'{param_name}: {param_type}' in hover['contents']['value'], hover
                assert hover['contents']['value'].split(f'`{param_name}: {param_type}`: ')[1].strip()
                argument_column = column + len(name) + 1
                argument = c.at('hover', line, argument_column)
                assert argument is not None
                assert 'Value passed to this builtin' not in argument['contents']['value']
                items = c.at('completion', line, argument_column)
                assert any(item['label'] == param_name for item in items)
        hello = (ROOT / 'examples/hello-world.propan').read_text()
        c.change(hello)
        assert 'value: int' in c.at('hover', 5, 26)['contents']['value']
        assert 'addr: address' in c.at('hover', 11, 29)['contents']['value']
        c.change(SOURCE)
        hover = c.at('hover', 7, 8)
        assert 'MOV' in hover['contents']['value']
        hover = c.at('hover', 7, 18)
        assert '42' in hover['contents']['value'] and 'int' in hover['contents']['value']
        assert hover['range']['start'] == {'line': 7, 'character': 18}
        assert '42' in c.at('hover', 4, 7)['contents']['value']
        assert 'UTF-8 string' in c.at('hover', 5, 19)['contents']['value']
        assert 'text' in c.at('hover', 5, 13)['contents']['value']
        assert 'Hub: 0x8' in c.at('hover', 7, 11)['contents']['value']
        assert 'PC/local: 0x2' in c.at('hover', 9, 5)['contents']['value']
        assert c.at('definition', 7, 18)['range']['start'] == {'line': 4, 'character': 6}
        assert c.at('definition', 7, 11)['range']['start'] == {'line': 9, 'character': 4}
        assert c.at('definition', 8, 12)['range']['start']['line'] == 8
        assert c.at('definition', 11, 12)['range']['start']['line'] == 11
        lenses = c.document('codeLens')
        assert any('Hub: 0x8' in lens['command']['title'] and '1 references' in lens['command']['title'] for lens in lenses)
        tokens = c.document('semanticTokens/full')['data']
        assert {0, 1, 2, 3} <= set(tokens[3::5])
        hints = check_inlay_hints(c, SOURCE, {
            7: '$00000 | 000 | ', 8: '$00004 | 001 | ',
            9: '$00008 | 002 | ', 10: '$0000C | 003 | ',
            11: '$00010 | 004 | ',
        })
        assert c.document('inlayHint', range={
            'start': {'line': 9, 'character': 0},
            'end': {'line': 9, 'character': 0}}) == [hints[9]]
        assert c.document('inlayHint', range={
            'start': {'line': 9, 'character': 1},
            'end': {'line': 9, 'character': 10}}) == []
        # Shared hub offsets at mode boundaries must use the label's own segment.
        modes = ('.cogexec\ncog: NOP\ncog_end:\n.hubexec\nhub:\nNOP\n'
                 '.lutexec\nlut: NOP\n.data\nbytes: BYTE 1, 2\n')
        c.change(modes)
        check_inlay_hints(c, modes, {
            1: '$00000 | 000 | ', 2: '$00004 | 001 | ',
            4: '$00004 |     | ', 5: '$00004 |     | ',
            7: '$00008 | 200 | ', 9: '$0000C |     | ',
        })
        registers = '.regspace\nvar slot:\nRES 1\n.cogexec\nempty_label:\n'
        c.change(registers)
        check_inlay_hints(c, registers, {
            1: '       | 000 | ', 4: '$00000 | 000 | ',
        })
        for empty in ('', '\n', '// comment\r\n\r\n   \r\nconst answer = 42\r\n'):
            c.change(empty)
            check_inlay_hints(c, empty, {})
        c.change(SOURCE + 'LONG missing_symbol\n')
        check_inlay_hints(c, SOURCE + 'LONG missing_symbol\n', {})
        c.change(SOURCE)
        edits = c.document('formatting', options={'tabSize': 4, 'insertSpaces': True})
        assert len(edits) == 1 and 'MOV' in edits[0]['newText']
        c.change(edits[0]['newText'])
        assert c.document('formatting', options={'tabSize': 4, 'insertSpaces': True}) == []

        items = c.complete('    ')
        assert {'MOV', '.cogexec', 'if(C)', 'return'} <= items.keys()
        assert set(directive_samples) <= items.keys()
        assert '.elseif' not in items
        assert 'cog RAM' in items['.cogexec']['documentation']['value']
        assert 'answer' not in items
        assert '.cogexec' in c.complete('    .co')
        assert 'MOV' in c.complete('    mo')
        assert {'answer', 'value', 'utf16', 'hubaddr'} <= c.complete('    MOV value, ').keys()
        assert 'MOV' not in c.complete('    MOV value, ')
        assert {':wc', ':wz', ':wcz'} == c.complete('    MOV value, answer :').keys()
        assert {':wc', ':wz', ':wcz'} == c.complete('    MOV value, answer ').keys()
        assert {':wc', ':wz', ':wcz'} == c.complete('    MOV value, answer :w').keys()
        assert {':wz'} == c.complete('    MOV value, answer :wz').keys()
        assert ':wz' not in c.complete('    MOV value, ')
        assert ':wz' not in c.complete('    MOV value, answer + ')
        assert not c.complete('    MOV value, answer :wc ')
        effect = c.complete('    MOV value, answer :w')[':wc']['textEdit']
        assert effect['newText'] == ':wc', effect
        assert effect['range']['start']['character'] == len('    MOV value, answer '), effect
        assert effect['range']['end']['character'] == len('    MOV value, answer :w'), effect
        existing_effect = '    MOV value, answer :wz'
        c.change(SOURCE + existing_effect)
        items = c.at('completion', SOURCE.count('\n'), len(existing_effect) - 1)
        replacement = next(item['textEdit'] for item in items if item['label'] == ':wc')
        assert replacement['range']['end']['character'] == len(existing_effect), replacement
        assert replacement['newText'] == ':wc', replacement
        effect = c.complete('    MOV value, aug(1234) ')[':wz']['textEdit']
        assert effect['newText'] == ':wz', effect
        assert effect['range']['start'] == effect['range']['end'], effect
        assert 'answer' not in c.complete('    MOV value, answer :')
        assert not c.complete('    NOP :')
        assert not c.complete('    NOP ')
        assert {'C', 'Z'} <= c.complete('    if(').keys()
        assert 'MOV' in c.complete('    if(C) ')
        assert 'MOV' in c.complete('new_label: ')
        assert 'MOV' in c.complete('var new_variable: ')
        assert ':wc' in c.complete('    if(C) MOV value, answer :')
        assert ':wz' in c.complete('    if(C) MOV value, answer ')
        assert ':wc' in c.complete('label: MOV value, answer ')
        assert ':wz' in c.complete('    return MOV value, answer ')
        assert '.loop' in c.complete('    JMP ')
        assert '.loop' not in c.complete('fresh_label: JMP ')
        assert '.loop' not in c.complete('.hubexec\nJMP ')
        assert 'text' in c.complete('const x = utf16(')
        assert c.complete('const x = utf16(te')['text']['textEdit']['newText'] == 'text='
        assert 'text' not in c.complete('const x = utf16(text="abc", ')
        c.change(SOURCE + 'const x = utf16(text="abc")')
        assert c.at('completion', SOURCE.count('\n'), len('const x = utf16(te'))[0]['textEdit']['newText'] == 'text'
        assert 'addr' in c.complete('const x = hubaddr(')
        assert 'addr' in c.complete('const x = hubaddr(\n    ')
        assert not c.complete('// MOV')
        assert not c.complete('const x = "MOV')
        assert not c.complete('const x = "unfinished' + chr(92))
        c.change(SOURCE + 'LONG missing_symbol\n')
        assert c.at('hover', SOURCE.count('\n'), 7) is None
        c.change(SOURCE + 'const broken = utf16(\n')
        assert c.at('definition', 7, 18)['range']['start']['line'] == 4
        assert c.document('formatting', options={'tabSize': 4, 'insertSpaces': True}) is None
        check_inlay_hints(c, SOURCE + 'const broken = utf16(\n', {})
        c.change(SOURCE)
        c.notify('textDocument/didChange', {'textDocument': {'uri': URI, 'version': 3}, 'contentChanges': [
            {'range': {'start': {'line': 4, 'character': 21}, 'end': {'line': 4, 'character': 15}}, 'text': 'bad'}]})
        assert '42' in c.at('hover', 7, 18)['contents']['value']
        c.notify('textDocument/didChange', {'textDocument': {'uri': URI, 'version': 3}, 'contentChanges': [
            {'range': {'start': {'line': 4, 'character': 15}, 'end': {'line': 4, 'character': 21}}, 'text': '99'}]})
        assert '99' in c.at('hover', 7, 18)['contents']['value']
        c.notify('textDocument/didClose', {'textDocument': {'uri': URI}})
        assert c.at('hover', 0, 0) is None
        assert c.document('inlayHint', range={
            'start': {'line': 0, 'character': 0},
            'end': {'line': 20, 'character': 0}}) is None
        c.open(SOURCE)
    finally:
        c.close()

    for encoding, units in [('utf-8', 4), ('utf-16', 2), ('utf-32', 1)]:
        c = Client(encoding)
        try:
            c.change('const greeting = "😀"\nconst a = 1\nLONG "😀", a\n')
            definition = c.at('definition', 2, 9 + units)
            assert definition['range']['start'] == {'line': 1, 'character': 6}
            c.notify('textDocument/didChange', {'textDocument': {'uri': URI, 'version': 3}, 'contentChanges': [
                {'range': {'start': {'line': 0, 'character': 18}, 'end': {'line': 0, 'character': 18 + units}}, 'text': 'x'}]})
            assert 'x' in c.at('hover', 0, 7)['contents']['value']
        finally:
            c.close()
    print('LSP protocol checks passed')


if __name__ == '__main__':
    signal.signal(signal.SIGALRM, lambda *_: sys.exit('LSP test timed out'))
    signal.alarm(45)
    run()
