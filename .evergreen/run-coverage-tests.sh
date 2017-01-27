#!/bin/bash

set -o xtrace   # Write all commands first to stderr
set -o errexit  # Exit the script with error if any of the commands fail

# Supported/used environment variables:
#       MONGODB_URI             Set the suggested connection MONGODB_URI (including credentials and topology info)
#       TOPOLOGY                Allows you to modify variables and the MONGODB_URI based on test topology
#                               Supported values: "server", "replica_set", "sharded_cluster"

MONGODB_URI=${MONGODB_URI:-}
TOPOLOGY=${TOPOLOGY:-server}

############################################
#            Main Program                  #
############################################
JAVA_HOME="/opt/java/jdk8"

# Provision the correct connection string and set up SSL if needed
if [ "$TOPOLOGY" == "sharded_cluster" ]; then

     if [ "$AUTH" = "auth" ]; then
       export MONGODB_URI="mongodb://bob:pwd123@localhost:27017/?authSource=admin"
     else
       export MONGODB_URI="mongodb://localhost:27017"
     fi
fi

JAVA_HOME="/opt/java/jdk8"
echo "Running unit tests for Scala $SCALA_VERSION"

./sbt -java-home $JAVA_HOME version
./sbt -java-home $JAVA_HOME coverage test -Dorg.mongodb.test.uri=${MONGODB_URI}
./sbt -java-home $JAVA_HOME coverage it:test -Dorg.mongodb.test.uri=${MONGODB_URI}
./sbt -java-home $JAVA_HOME coverageAggregate
./sbt -java-home $JAVA_HOME coverageReport
