# Script to enable UTF-8 output in SBT / PowerShell

# Better alternative to the `chcp 65001` command for enabling UTF-8 output in PowerShell,
# since apparently that command only sets the *input* encoding
# See https://github.com/sbt/sbt/issues/4322#issuecomment-1205912297
$OutputEncoding = [console]::InputEncoding = [console]::OutputEncoding = New-Object System.Text.UTF8Encoding

# See the next comment in that same Issue thread; JDK >= 19 adds new properties for I/O encoding
$env:JAVA_TOOL_OPTIONS='-Dstdout.encoding=UTF-8 -Dstderr.encoding=UTF-8'
