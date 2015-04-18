@ECHO OFF
SET PATH=%PATH%;C:\Program Files (x86)\WinSCP
chcp 65001
cabal build
dist\build\site\site.exe build
dist\build\site\site.exe deploy