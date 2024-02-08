package maf.save

import io.bullet.borer.Decoder

trait LoadMapToArray:
    given mapKeyDecoder[K, V](using keyDecoder: Decoder[K], valueDecoder: Decoder[V]): EncapsulatedDecoder[Map[K, V]]
