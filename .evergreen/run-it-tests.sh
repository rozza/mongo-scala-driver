#!/bin/bash

set -o xtrace   # Write all commands first to stderr
set -o errexit  # Exit the script with error if any of the commands fail

# Supported/used environment variables:
#       MONGODB_URI             Set the suggested connection MONGODB_URI (including credentials and topology info)
#       TOPOLOGY                Allows you to modify variables and the MONGODB_URI based on test topology
#                               Supported values: "server", "replica_set", "sharded_cluster"
#       SCALA_VERSION           Set the version of Scala to be used.


MONGODB_URI=${MONGODB_URI:-}
TOPOLOGY=${TOPOLOGY:-server}



############################################
#            Main Program                  #
############################################

# Provision the correct connection string and set up SSL if needed
if [ "$TOPOLOGY" == "sharded_cluster" ]; then

     if [ "$AUTH" = "auth" ]; then
       export MONGODB_URI="mongodb://bob:pwd123@localhost:27017/?authSource=admin"
     else
       export MONGODB_URI="mongodb://localhost:27017"
     fi
fi

echo "Running Integration tests for Scala $SCALA_VERSION, $AUTH tests over $SSL for $TOPOLOGY and connecting to $MONGODB_URI"
export JAVA_HOME="/opt/java/jdk8"

./sbt -java-home $JAVA_HOME version
./sbt -java-home $JAVA_HOME ++${SCALA_VERSION} it:test -Dorg.mongodb.test.uri=${MONGODB_URI}
