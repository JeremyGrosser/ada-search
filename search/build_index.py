import sqlite3
import os

basedir = '../alire-20220425'
os.unlink('index.db')
db = sqlite3.connect('index.db')
db.execute('CREATE VIRTUAL TABLE f USING fts5(crate, path, filename, text);')

rowid = 0
for crate in os.listdir(basedir):
    print(crate)
    for dirpath, dirnames, filenames in os.walk(os.path.join(basedir, crate)):
        for filename in filenames:
            path = os.path.join(dirpath, filename)
            if path[-4:] not in ('.ads', '.adb'):
                continue
            try:
                with open(path, 'rb') as fd:
                    text = fd.read()
                    db.execute('INSERT INTO f(rowid, crate, path, filename, text) VALUES (?, ?, ?, ?, ?)', (rowid, crate.encode('utf-8'), path.encode('utf-8'), filename.encode('utf-8'), text))
                    rowid += 1
            except Exception as e:
                print(str(e))
db.commit()
db.execute('pragma optimize')
db.close()
