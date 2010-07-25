# MAVEN needs to point to a MAVEN3 installation:
if which mvn >/dev/null; then
  mvn -version | grep "Maven 3" > /dev/null
  if [ $? -eq 0 ]; then
    MAVEN="mvn"
  fi
fi

if [ "X$MAVEN" = "X" ] ; then
  echo "Missing environment variable \"MAVEN\". This has to point to a maven 3.0 installation, "
  echo "e.g. add the following line to your .bashrc (and make sure the path is correct):"
  echo "export MAVEN=/opt/apache-maven-3.0-beta-1/bin/mvn"
  exit
fi

set_toolchain_version()
{
    ${MAVEN} -f toolchain-pom.xml -N versions:set -DnewVersion=$1
    ${MAVEN} -f toolchain-pom.xml -N versions:update-child-modules
}

build_both()
{
    ${MAVEN} \
	-U \
        -Dscala.toolchain.version=${SCALA_TOOLCHAIN_VERSION} \
        -Dscala.version=${SCALA_VERSION} \
        -f toolchain-pom.xml \
        clean install $* && \
    ${MAVEN} \
	-U \
        -Dscala.toolchain.version=${SCALA_TOOLCHAIN_VERSION} \
        -Dscala.version=${SCALA_VERSION} \
        clean install $*
}
