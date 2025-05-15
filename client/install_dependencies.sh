mvn install:install-file \
  -Dfile=sound/library/tritonus-share-0.3.7.4.jar \
  -DgroupId=com.googlecode.soundlibs \
  -DartifactId=tritonus-share \
  -Dversion=0.3.7.4 \
  -Dpackaging=jar

mvn install:install-file \
  -Dfile=sound/library/jlayer-1.0.1.4.jar \
  -DgroupId=com.googlecode.soundlibs \
  -DartifactId=jl \
  -Dversion=1.0.1.4 \
  -Dpackaging=jar

mvn install:install-file \
  -Dfile=sound/library/mp3spi-1.9.5.4.jar \
  -DgroupId=com.googlecode.soundlibs \
  -DartifactId=mp3spi \
  -Dversion=1.9.5.4 \
  -Dpackaging=jar

mvn install:install-file \
  -Dfile=sound/library/jsyn-17.1.0.jar \
  -DgroupId=com.jsyn \
  -DartifactId=jsyn \
  -Dversion=17.1.0 \
  -Dpackaging=jar

mvn install:install-file \
  -Dfile=sound/library/sound.jar \
  -DgroupId=org.processing \
  -DartifactId=sound \
  -Dversion=2.4.0 \
  -Dpackaging=jar

mvn install:install-file \
  -Dfile=sound/library/jportaudio.jar \
  -DgroupId=com.portaudio \
  -DartifactId=jportaudio \
  -Dversion=1.0 \
  -Dpackaging=jar