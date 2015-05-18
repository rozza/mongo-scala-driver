package com.mongodb.scala.core

import java.util.Arrays._

import org.bson.codecs.configuration.CodecRegistries._
import org.bson.codecs.configuration.CodecRegistry
import org.bson.codecs.{BsonValueCodecProvider, DocumentCodecProvider => ADocumentCodecProvider, ValueCodecProvider}

package object codecs {
  val DEFAULT_CODEC_REGISTRY: CodecRegistry = fromProviders(asList(DocumentCodecProvider(),
    new ValueCodecProvider, new ADocumentCodecProvider, new BsonValueCodecProvider))
}
