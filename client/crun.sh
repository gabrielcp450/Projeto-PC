#!/bin/bash
mvn clean package && java -cp target/duelo-client-1.0-SNAPSHOT.jar com.duelo.client.Main

