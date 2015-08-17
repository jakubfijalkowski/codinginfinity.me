#!/bin/bash
lftp -c "set ssl:verify-certificate no;
open 'ftp.codinginfinity.me';
user '$FTP_USERNAME' '$FTP_PASSWORD';
lcd _site;
mirror --reverse --ignore-time . .;
bye; " > /dev/null
