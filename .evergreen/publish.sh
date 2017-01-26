#!/bin/bash

# DO NOT ECHO COMMANDS AS THEY CONTAIN SECRETS!

set -o errexit  # Exit the script with error if any of the commands fail

############################################
#            Main Program                  #
############################################

JAVA_HOME="/opt/java/jdk8"
PUBLISH_PROPERTIES_FILE=${PROJECT_DIRECTORY}/.publishProperties

echo ${RING_FILE_GPG_BASE64} | base64 -d > ${PROJECT_DIRECTORY}/secring.gpg

echo nexusUsername=${NEXUS_USERNAME} > $PUBLISH_PROPERTIES_FILE
echo nexusPassword=${NEXUS_PASSWORD} >> $PUBLISH_PROPERTIES_FILE
echo signing.keyId=${SIGNING_KEY_ID} >> $PUBLISH_PROPERTIES_FILE
echo signing.password=${SIGNING_PASSWORD} >> $PUBLISH_PROPERTIES_FILE
echo signing.secretKeyRingFile=${PROJECT_DIRECTORY}/secring.gpg >> $PUBLISH_PROPERTIES_FILE

echo "Publishing snapshots"


./sbt -java-home $JAVA_HOME +clean +publishSnapshot -DpublishProperties=$PUBLISH_PROPERTIES_FILE
