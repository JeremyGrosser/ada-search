import wsgiref.simple_server
import urllib.parse
import sqlite3
import os.path


db = sqlite3.connect('file:index.db?mode=ro', uri=True)
basedir = '/archive/u1/src/alire-backup'


def search(query):
    seen = set()
    head = open('head.html', 'r').read()
    tail = open('tail.html', 'r').read()
    cur = db.cursor()
    cur.execute('SELECT crate, filename, path, rank FROM f WHERE text MATCH ? GROUP BY crate ORDER BY rank', (query,))
    yield head.format(query=query).encode('utf-8')
    for crate, filename, path, rank in cur.fetchall():
        key = (crate, filename)
        if key in seen:
            continue
        else:
            seen.add(key)

        yield b'<div class="result">\n'

        path = path.lstrip(b'./')
        link = '{crate} <a href="/source/{path}">{filename}</a>\n'.format(
                path=path.decode('utf-8'),
                crate=crate.decode('utf-8'),
                filename=filename.decode('utf-8'))
        yield link.encode('utf-8')

        #yield b'<pre class="text">'
        #yield text
        #yield b'</pre>\n'

        yield b'</div>\n'

    if len(seen) == 0:
        yield b'No results!'

    yield tail.encode('utf-8')
    return


def index():
    return [open('index.html', 'rb').read()]


def make_headers(content_type):
    return [
        ('Content-type', content_type),
    ]


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
        elif environ['PATH_INFO'].startswith('/source/'):
            try:
                path = environ['PATH_INFO'].split('/', 2)[-1]
                path = os.path.abspath(os.path.join(basedir, path))
                if not path.startswith(basedir) or not (path.endswith('.ads') or path.endswith('.adb')):
                    start_response('400 Bad Request', make_headers('text/plain;charset=utf-8'))
                    return [b'400 Bad Request\r\n']
                else:
                    with open(path, 'rb') as fd:
                        start_response('200 OK', make_headers('text/plain;charset=utf-8'))
                        return [fd.read()]
            except Exception as e:
                print(str(e))
                start_response('400 Bad Request', make_headers('text/plain;charset=utf-8'))
                return [b'400 Bad Request\r\n']
        elif environ['PATH_INFO'] == '/ada.svg':
            start_response('200 OK', make_headers('image/svg+xml'))
            return [open('ada.svg', 'rb').read()]
    
    start_response('404 Not Found', make_headers('text/plain;charset=utf-8'))
    return [b'404 Not Found\r\n']


if __name__ == '__main__':
    server = wsgiref.simple_server.make_server('', 8000, application)
    server.serve_forever()
