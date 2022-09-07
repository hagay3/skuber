package skuber.apps.v1

/**
 * @author David O'Riordan
 */

import skuber.ResourceSpecification.{Names, Scope}
import skuber._

import play.api.libs.functional.syntax._
import play.api.libs.json.{Format, JsPath, Json}
import skuber.json.format._ // reuse some core skuber json formatters

case class StatefulSet(override val kind: String = "StatefulSet",
                       override val apiVersion: String = appsAPIVersion,
                       metadata: ObjectMeta,
                       spec: Option[StatefulSet.Spec] = None,
                       status: Option[StatefulSet.Status] = None) extends ObjectResource {
  def withResourceVersion(version: String): StatefulSet = this.copy(metadata = metadata.copy(resourceVersion = version))

  lazy val copySpec: StatefulSet.Spec = this.spec.getOrElse(new StatefulSet.Spec(template = Pod.Template.Spec()))
  private val rollingUpdateStrategy = StatefulSet.UpdateStrategy(`type` = StatefulSet.UpdateStrategyType.RollingUpdate, None)

  private def rollingUpdateStrategy(partition: Int) =
    StatefulSet.UpdateStrategy(`type` = StatefulSet.UpdateStrategyType.RollingUpdate, Some(StatefulSet.RollingUpdateStrategy(partition)))

  def withReplicas(count: Int): StatefulSet = this.copy(spec = Some(copySpec.copy(replicas = Some(count))))

  def withServiceName(serviceName: String): StatefulSet = this.copy(spec = Some(copySpec.copy(serviceName = Some(serviceName))))

  def withTemplate(template: Pod.Template.Spec): StatefulSet = this.copy(spec = Some(copySpec.copy(template = template)))

  def withLabelSelector(sel: LabelSelector): StatefulSet = this.copy(spec = Some(copySpec.copy(selector = Some(sel))))

  def withRollingUpdateStrategyPartition(partition: Int): StatefulSet = this.copy(spec = Some(copySpec.copy(updateStrategy = Some(rollingUpdateStrategy(partition)))))

  def withVolumeClaimTemplate(claim: PersistentVolumeClaim): StatefulSet = {
    val spec = copySpec.withVolumeClaimTemplate(claim)
    this.copy(spec = Some(spec))
  }
}

object StatefulSet {

  val specification: NonCoreResourceSpecification = NonCoreResourceSpecification(apiGroup = "apps",
    version = "v1",
    scope = Scope.Namespaced,
    names = Names(plural = "statefulsets",
      singular = "statefulset",
      kind = "StatefulSet",
      shortNames = List()))
  implicit val stsDef: ResourceDefinition[StatefulSet] = new ResourceDefinition[StatefulSet] {
    def spec: ResourceSpecification = specification
  }
  implicit val stsListDef: ResourceDefinition[StatefulSetList] = new ResourceDefinition[StatefulSetList] {
    def spec: ResourceSpecification = specification
  }
  implicit val scDef: Scale.SubresourceSpec[StatefulSet] = new Scale.SubresourceSpec[StatefulSet] {
    override def apiVersion: String = appsAPIVersion
  }

  def apply(name: String): StatefulSet = StatefulSet(metadata = ObjectMeta(name = name))

  object PodManagementPolicyType extends Enumeration {
    type PodManagementPolicyType = Value
    val OrderedReady, Parallel = Value
  }

  object UpdateStrategyType extends Enumeration {
    type UpdateStrategyType = Value
    val OnDelete, RollingUpdate = Value
  }

  case class UpdateStrategy(`type`: UpdateStrategyType.UpdateStrategyType, rollingUpdate: Option[RollingUpdateStrategy] = None)

  case class RollingUpdateStrategy(partition: Int)

  case class Spec(replicas: Option[Int] = Some(1),
                  serviceName: Option[String] = None,
                  selector: Option[LabelSelector] = None,
                  template: Pod.Template.Spec,
                  volumeClaimTemplates: List[PersistentVolumeClaim] = Nil,
                  podManagementPolicy: Option[PodManagementPolicyType.PodManagementPolicyType] = None,
                  updateStrategy: Option[UpdateStrategy] = None,
                  revisionHistoryLimit: Option[Int] = None) {
    def withVolumeClaimTemplate(claim: PersistentVolumeClaim): Spec = copy(volumeClaimTemplates = claim :: volumeClaimTemplates)
  }

  case class Condition(`type`: String, status: String, lastTransitionTime: Option[Timestamp], reason: Option[String], message: Option[String])

  case class Status(observedGeneration: Option[Int],
                    replicas: Int,
                    readyReplicas: Option[Int],
                    updatedReplicas: Option[Int],
                    currentRevision: Option[String],
                    updateRevision: Option[String],
                    collisionCount: Option[Int],
                    conditions: Option[List[Condition]])

  // json formatters

  implicit val statefulSetPodPcyMgmtFmt: Format[StatefulSet.PodManagementPolicyType.PodManagementPolicyType] = enumDefault(StatefulSet.PodManagementPolicyType, StatefulSet.PodManagementPolicyType.OrderedReady.toString)


  implicit val statefulSetRollUp: Format[StatefulSet.RollingUpdateStrategy] = Json.format[StatefulSet.RollingUpdateStrategy]
  implicit val statefulSetUpdStrFmt: Format[StatefulSet.UpdateStrategy] = ((JsPath \ "type").formatEnum(StatefulSet.UpdateStrategyType, StatefulSet.UpdateStrategyType.RollingUpdate.toString) and
    (JsPath \ "rollingUpdate").formatNullable[StatefulSet.RollingUpdateStrategy]) (StatefulSet.UpdateStrategy.apply, st => (st.`type`, st.rollingUpdate))

  implicit val statefulSetSpecFmt: Format[StatefulSet.Spec] = ((JsPath \ "replicas").formatNullable[Int] and
    (JsPath \ "serviceName").formatNullable[String] and
    (JsPath \ "selector").formatNullableLabelSelector and
    (JsPath \ "template").format[Pod.Template.Spec] and
    (JsPath \ "volumeClaimTemplates").formatMaybeEmptyList[PersistentVolumeClaim] and
    (JsPath \ "podManagementPolicy").formatNullableEnum(StatefulSet.PodManagementPolicyType) and
    (JsPath \ "updateStrategy").formatNullable[StatefulSet.UpdateStrategy] and
    (JsPath \ "revisionHistoryLimit").formatNullable[Int]) (StatefulSet.Spec.apply,
    sp => (sp.replicas, sp.serviceName, sp.selector, sp.template, sp.volumeClaimTemplates, sp.podManagementPolicy, sp.updateStrategy, sp.revisionHistoryLimit))

  implicit val statefulSetCondFmt: Format[StatefulSet.Condition] = Json.format[StatefulSet.Condition]
  implicit val statefulSetStatusFmt: Format[StatefulSet.Status] = Json.format[StatefulSet.Status]

  implicit lazy val statefulSetFormat: Format[StatefulSet] = (objFormat and
    (JsPath \ "spec").formatNullable[StatefulSet.Spec] and
    (JsPath \ "status").formatNullable[StatefulSet.Status]) (StatefulSet.apply, st => (st.kind, st.apiVersion, st.metadata, st.spec, st.status))

  implicit val statefulSetListFormat: Format[StatefulSetList] = ListResourceFormat[StatefulSet]
}

