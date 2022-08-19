#!/bin/sh
set -e
set -x
mvn dependency:build-classpath "-Dmdep.outputFile=cp.txt"

( 
    echo '#!/bin/sh'
    echo 'set -e'
    echo 'cd $(dirname "$0")'
    echo "exec java -Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=8887 -cp $(cat cp.txt):target/classes:  io.opsit.explang.parser.alg.Main \"\$@\""
) > runrepl.sh
rm cp.txt
chmod 755 runrepl.sh 
