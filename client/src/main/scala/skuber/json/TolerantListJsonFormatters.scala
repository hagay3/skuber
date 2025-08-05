package skuber.json

import play.api.libs.functional.FunctionalBuilder
import play.api.libs.functional.syntax._
import play.api.libs.json._
import skuber.json.format._
import skuber.{ConfigMap, ConfigMapList, Endpoints, EndpointsList, Event, EventList, LimitRange, LimitRangeList, ListMeta, ListResource, Namespace, NamespaceList, Node, NodeList, ObjectResource, PersistentVolume, PersistentVolumeClaim, PersistentVolumeClaimList, PersistentVolumeList, Pod, PodList, ReplicationController, ReplicationControllerList, Resource, ResourceQuotaList, SecretList, Service, ServiceAccount, ServiceAccountList, ServiceList}
/**
  * Tolelrant list reads that will not fail if some of the elements of the list do not validate against the Reads[T]
  */

package object TolerantListJsonFormatters {

  implicit def tolerantListFormat[T](implicit r: Reads[T], w: Writes[T]): Format[List[T]] = {
    val tolerantReads: Reads[List[T]] =
      Reads.list[JsValue].map(_.flatMap(_.validate[T].asOpt)) // drop bad items
    Format(tolerantReads, Writes.list[T](w))
  }

  private def formatMaybeEmptyTolerantList[T](path: JsPath)
                                             (implicit tReads: Reads[T], tWrites: Writes[T], omitEmpty: Boolean = true
                                     ): OFormat[List[T]] = {
    path
      .formatNullable[List[T]] // picks up implicit Format[List[T]] above
      .inmap[List[T]](
        _.getOrElse(Nil),
        l => if (omitEmpty && l.isEmpty) None else Some(l)
      )
  }



  def listTolerantResourceFormatBuilder[O <: ObjectResource](implicit f: Format[O]): FunctionalBuilder[OFormat]#CanBuild4[String, String, Option[ListMeta], List[O]] = {
    (JsPath \ "apiVersion").format[String] and
      (JsPath \ "kind").format[String] and
      (JsPath \ "metadata").formatNullable[ListMeta] and
      formatMaybeEmptyTolerantList[O](JsPath \ "items")
  }


  def ListTolerantResourceFormat[O <: ObjectResource](implicit f: Format[O]): OFormat[ListResource[O]] = listTolerantResourceFormatBuilder[O].apply(ListResource.apply,
    l => (l.apiVersion, l.kind, l.metadata, l.items))

  /**
    * * Tolerant list formats for skuber resources.
    * The goal of those formatters is to fail silently in case one of the items in the list does not validate against the Reads[T].
    * It's helpful in case new kubernetes resources are added to the cluster and skuber does not know about them yet.
    */
  implicit val podListFmtTolerant: Format[PodList] = ListTolerantResourceFormat[Pod]
  implicit val nodeListFmtTolerant: Format[NodeList] = ListTolerantResourceFormat[Node]
  implicit val configMapListFmtTolerant: Format[ConfigMapList] = ListTolerantResourceFormat[ConfigMap]
  implicit val serviceListFmtTolerant: Format[ServiceList] = ListTolerantResourceFormat[Service]
  implicit val endpointsListFmtTolerant: Format[EndpointsList] = ListTolerantResourceFormat[Endpoints]
  implicit val eventListFmtTolerant: Format[EventList] = ListTolerantResourceFormat[Event]
  implicit val namespaceListFmtTolerant: Format[NamespaceList] = ListTolerantResourceFormat[Namespace]
  implicit val replCtrlListFmtTolerant: Format[ReplicationControllerList] = ListTolerantResourceFormat[ReplicationController]
  implicit val persVolListFmtTolerant: Format[PersistentVolumeList] = ListTolerantResourceFormat[PersistentVolume]
  implicit val persVolClaimListFmtTolerant: Format[PersistentVolumeClaimList] = ListTolerantResourceFormat[PersistentVolumeClaim]
  implicit val svcAcctListFmtTolerant: Format[ServiceAccountList] = ListTolerantResourceFormat[ServiceAccount]
  implicit val resQuotaListFmtTolerant: Format[ResourceQuotaList] = ListTolerantResourceFormat[Resource.Quota]
  implicit val secretListFmtTolerant: Format[SecretList] = ListTolerantResourceFormat[skuber.Secret]
  implicit val limitRangeListFmtTolerant: Format[LimitRangeList] = ListTolerantResourceFormat[LimitRange]

}
