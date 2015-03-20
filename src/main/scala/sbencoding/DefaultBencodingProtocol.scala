package sbencoding

/**
 * User: zhaoyao
 * Date: 3/17/15
 * Time: 22:26
 */
trait DefaultBencodingProtocol
    extends BasicFormats
    with StandardFormats
    with CollectionFormats
    with ProductFormats
    with AdditionalFormats {

}

object DefaultBencodingProtocol extends DefaultBencodingProtocol
