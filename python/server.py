#
#  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
#
#  SPDX-License-Identifier: AGPL-3.0-or-later
#
from pygments import highlight
from pygments.lexers import AdaLexer
from pygments.formatters import HtmlFormatter
from pygments.styles import get_style_by_name

from html.parser import HTMLParser
from io import StringIO
import wsgiref.simple_server
import urllib.parse
import sqlite3
import os.path
import traceback
import sys


basedir = '/home/search/alire-backup'
db = sqlite3.connect('file:index.db?mode=ro', uri=True)

lexer = AdaLexer()
highlight_style = get_style_by_name('tango')


class MLStripper(HTMLParser):
    def __init__(self):
        super().__init__()
        self.reset()
        self.strict = False
        self.convert_charrefs= True
        self.text = StringIO()
    def handle_data(self, d):
        self.text.write(d)
    def get_data(self):
        return self.text.getvalue()


def strip_tags(html):
    s = MLStripper()
    s.feed(html)
    return s.get_data()


def search(query):
    seen = set()
    head = open('html/head.html', 'r').read()
    tail = open('html/tail.html', 'r').read()
    query = strip_tags(query.strip('"\''))
    yield head.format(query=query).encode('utf-8')
    cur = db.cursor()
    try:
        cur.execute('SELECT crate, filename, path, rank FROM f WHERE text MATCH ? GROUP BY crate, filename ORDER BY rank LIMIT 250', ('"%s"' % query,))
    except Exception as e:
        print('malformed query', repr(query), str(e))
        yield b'Malformed query'
        return

    for crate, filename, path, rank in cur.fetchall():
        key = path
        if key in seen:
            continue
        else:
            seen.add(key)

        yield b'<div class="result">\n'

        path = path.lstrip(b'./')
        link = '{crate} <a href="/source/{path}.html">{filename}</a>\n'.format(
                path=path.decode('utf-8'),
                crate=crate.decode('utf-8'),
                filename=filename.decode('utf-8'))
        yield link.encode('utf-8')

        #yield b'<pre class="text">'
        #yield text
        #yield b'</pre>\n'

        yield b'</div>\n'
    cur.close()

    if len(seen) == 0:
        yield b'No results!'

    yield tail.encode('utf-8')
    return


def index():
    return [open('html/index.html', 'rb').read()]


def make_headers(content_type):
    return [
        ('Content-Type', content_type),
        ('Cache-Control', 'public, max-age=604800')
    ]


def allowed_extension(path):
    ext = path.rsplit('.', 1)[-1]
    return ext in ('html', 'ads', 'adb')


def application(environ, start_response):
    if environ['REQUEST_METHOD'] == 'GET':
        if environ['PATH_INFO'] == '/':
            try:
                query = urllib.parse.parse_qs(environ['QUERY_STRING'])
                start_response('200 OK', make_headers('text/html;charset=utf-8'))
                q = query.get('q', None)
                if q:
                    return search(q[0])
                else:
                    return index()
            except Exception as e:
                print(str(e))
                start_response('400 Bad Request', make_headers('text/plain;charset=utf-8'))
                return [b'400 Bad Request\r\n']
        elif environ['PATH_INFO'].startswith('/source/alire-'):
            try:
                path = environ['PATH_INFO'].split('/', 2)[-1]
                path = os.path.abspath(os.path.join(basedir, path))
                if not path.startswith(basedir) or not allowed_extension(path):
                    start_response('404 Not Found', make_headers('text/plain;charset=utf-8'))
                    return [b'404 Not Found\r\n']
                else:
                    if path.endswith('.html'):
                        path = path.rsplit('.', 1)[0]
                        if not os.path.exists(path):
                            start_response('404 Not Found', make_headers('text/plain;charset=utf-8'))
                            return [b'404 Not Found\r\n']

                        query = urllib.parse.parse_qs(environ['QUERY_STRING'])
                        lineno = query.get('lineno', None)
                        with open(path, 'rb') as fd:
                            if lineno is not None:
                                hl_lines = [int(lineno[0])]
                            else:
                                hl_lines = []
                            formatter = HtmlFormatter(linenos=True, cssclass="source", hl_lines=hl_lines, style=highlight_style)
                            html = highlight(fd.read(), lexer, formatter)
                                
                            template = open('html/highlight.html', 'r').read()
                            filename = path[len(basedir):].split('/', 2)[-1]
                            raw = environ['PATH_INFO'].rsplit('.', 1)[0]
                            data = template.format(filename=filename, raw=raw, code=html)
                            start_response('200 OK', make_headers('text/html;charset=utf-8'))
                            return [data.encode('utf-8')]
                    else:
                        if not os.path.exists(path):
                            start_response('404 Not Found', make_headers('text/plain;charset=utf-8'))
                            return [b'404 Not Found\r\n']

                        with open(path, 'rb') as fd:
                            start_response('200 OK', make_headers('text/plain;charset=utf-8'))
                            return [fd.read()]
            except Exception as e:
                traceback.print_tb(sys.exc_info()[2])
                print(str(e))
                start_response('500 Internal Server Error', make_headers('text/plain;charset=utf-8'))
                return [b'500 Internal Server Error\r\n']
        elif environ['PATH_INFO'] == '/ada.svg':
            start_response('200 OK', make_headers('image/svg+xml'))
            return [open('res/ada.svg', 'rb').read()]
        elif environ['PATH_INFO'] == '/ada-dark.svg':
            start_response('200 OK', make_headers('image/svg+xml'))
            return [open('res/ada-dark.svg', 'rb').read()]
        elif environ['PATH_INFO'] == '/search.css':
            start_response('200 OK', make_headers('text/css;charset=utf-8'))
            return [open('res/search.css', 'rb').read()]
        elif environ['PATH_INFO'] == '/robots.txt':
            start_response('200 OK', make_headers('text/plain;charset=utf-8'))
            return [open('res/robots.txt', 'rb').read()]
        start_response('404 Not Found', make_headers('text/plain;charset=utf-8'))
        return [b'404 Not Found\r\n']
    else:
        start_response('405 Method Not Allowed', make_headers('text/plain;charset=utf-8'))
        return [b'405 Method Not Allowed\r\n']


if __name__ == '__main__':
    server = wsgiref.simple_server.make_server('', 8000, application)
    server.serve_forever()
    #db.close()
