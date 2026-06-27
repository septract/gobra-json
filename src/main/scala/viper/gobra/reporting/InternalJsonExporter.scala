// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package viper.gobra.reporting

import org.json4s.JsonAST._
import org.json4s.native.JsonMethods.{compact, render}
import viper.gobra.ast.internal
import viper.gobra.ast.internal._
import viper.gobra.frontend.info.base.BuiltInMemberTag.BuiltInMemberTag
import viper.gobra.theory.Addressability
import viper.gobra.util.TypeBounds.{BoundedIntegerKind, IntegerKind, UnboundedInteger}
import viper.gobra.util.{BackendAnnotation, Binary, Decimal, Hexadecimal, NumBase, Octal}
import viper.silver.ast.{AbstractSourcePosition, HasLineColumn}

object InternalJsonExporter {

  final class UnsupportedInternalJson(message: String) extends RuntimeException(message)

  private sealed trait ExportValue
  private final case class ExportObject(fields: Vector[ExportField]) extends ExportValue
  private final case class ExportArray(values: Vector[ExportValue]) extends ExportValue
  private final case class ExportString(value: String) extends ExportValue
  private final case class ExportBool(value: Boolean) extends ExportValue
  private final case class ExportInteger(value: BigInt) extends ExportValue
  private case object ExportNull extends ExportValue

  private final case class ExportField(name: String, value: ExportValue)

  private def fail(message: String): Nothing =
    throw new UnsupportedInternalJson(message)

  def format(program: internal.Program, inputs: Vector[String]): String =
    compact(render(toJson(document(program, inputs)))) + "\n"

  private def document(program: internal.Program, inputs: Vector[String]): ExportValue =
    obj(
      "schema" -> obj(
        "name" -> str("gobra.internal"),
        "version" -> int(1),
        "encoding" -> str("structural-adt"),
        "failClosed" -> bool(true)
      ),
      "inputs" -> arr(inputs.map(str)),
      "program" -> encodeProgram(program)
    )

  private def encodeProgram(program: internal.Program): ExportValue =
    nodeObj("Program", program.info,
      "types" -> arr(program.types.map(encodeTopType)),
      "members" -> arr(program.members.map(encodeNode))
    )

  private def encodeNode(node: Node): ExportValue = node match {
    case p: internal.Program => encodeProgram(p)
    case _ =>
      val tag = node.getClass.getSimpleName
      if (!knownNodeTags.contains(tag)) {
        fail(s"cannot export unsupported internal node constructor ${node.getClass.getName}")
      }
      val names = node.productElementNames.toVector
      val values = node.productIterator.toVector
      if (names.length != values.length) {
        fail(s"cannot export ${node.getClass.getName}: product field names do not match product arity")
      }
      val fields = names.zip(values).flatMap {
        case ("info", _: Source.Parser.Info) => None
        case (name, value) => Some(name -> encodeAny(value, s"${node.getClass.getSimpleName}.$name"))
      }
      nodeObj(tag, node.info, fields: _*)
  }

  private val knownNodeTags: Set[String] = Set(
    "Access",
    "Add",
    "Address",
    "AdtClause",
    "AdtClauseProxy",
    "AdtConstructorLit",
    "AdtDefinition",
    "AdtDestructor",
    "AdtDiscriminator",
    "And",
    "ApplyWand",
    "ArrayLit",
    "ArrayTExpr",
    "ArrayUpdate",
    "Assert",
    "AssertByContra",
    "AssertByProof",
    "Asserting",
    "AssignSuchThat",
    "Assume",
    "AtLeastCmp",
    "AtMostCmp",
    "BitAnd",
    "BitClear",
    "BitNeg",
    "BitOr",
    "BitXor",
    "Block",
    "BoolLit",
    "BoolTExpr",
    "BoundVar",
    "Break",
    "BuiltInFPredicate",
    "BuiltInFunction",
    "BuiltInMPredicate",
    "BuiltInMethod",
    "Capacity",
    "ClosureCall",
    "ClosureImplements",
    "ClosureObject",
    "ClosureSpec",
    "Conditional",
    "Contains",
    "Continue",
    "Conversion",
    "CurrentPerm",
    "DefinedTExpr",
    "Defer",
    "Deref",
    "DfltVal",
    "Div",
    "DomainAxiom",
    "DomainDefinition",
    "DomainFunc",
    "DomainFuncProxy",
    "DomainFunctionCall",
    "EffectfulConversion",
    "EqCmp",
    "Exhale",
    "Exists",
    "ExprAccess",
    "ExprAssertion",
    "FPredicate",
    "FPredicateAccess",
    "FPredicateProxy",
    "Field",
    "FieldRef",
    "Float32TExpr",
    "Float64TExpr",
    "Fold",
    "FractionalPerm",
    "FullPerm",
    "Function",
    "FunctionCall",
    "FunctionLit",
    "FunctionLitProxy",
    "FunctionObject",
    "FunctionProxy",
    "GhostCollectionUpdate",
    "GhostEqCmp",
    "GhostUneqCmp",
    "GlobalConstDecl",
    "GlobalVar",
    "GlobalVarDecl",
    "GlobalVarProxy",
    "GoClosureCall",
    "GoFunctionCall",
    "GoMethodCall",
    "GoSliceAppend",
    "GoSliceCopy",
    "GreaterCmp",
    "If",
    "Implication",
    "In",
    "IndexedExp",
    "Inhale",
    "Initialization",
    "IntLit",
    "IntTExpr",
    "Index",
    "Intersection",
    "IsBehaviouralSubtype",
    "IsComparableInterface",
    "IsComparableType",
    "ItfMethodWildcardMeasure",
    "ItfTupleTerminationMeasure",
    "Label",
    "LabelProxy",
    "LabeledOld",
    "Length",
    "LessCmp",
    "Let",
    "LocalVar",
    "Low",
    "LowContext",
    "MPredicate",
    "MPredicateAccess",
    "MPredicateProxy",
    "MagicWand",
    "MakeChannel",
    "MakeMap",
    "MakeSlice",
    "MapConversion",
    "MapKeys",
    "MapTExpr",
    "MapValues",
    "MathMapLit",
    "MathMapTExpr",
    "MatchAdt",
    "MatchBindVar",
    "MatchValue",
    "MatchWildcard",
    "MemoryPredicateAccess",
    "Method",
    "MethodBody",
    "MethodBodySeqn",
    "MethodCall",
    "MethodObject",
    "MethodProxy",
    "MethodSubtypeProof",
    "Mod",
    "Mul",
    "MultisetConversion",
    "MultisetLit",
    "MultisetTExpr",
    "Negation",
    "New",
    "NewMapLit",
    "NewSliceLit",
    "NilLit",
    "NoPerm",
    "NonItfMethodWildcardMeasure",
    "NonItfTupleTerminationMeasure",
    "Old",
    "OptionGet",
    "OptionNone",
    "OptionSome",
    "OptionTExpr",
    "Or",
    "Out",
    "Outline",
    "PackageWand",
    "PatternMatchAss",
    "PatternMatchCaseAss",
    "PatternMatchCaseExp",
    "PatternMatchCaseStmt",
    "PatternMatchExp",
    "PatternMatchStmt",
    "PermAdd",
    "PermDiv",
    "PermGeCmp",
    "PermGtCmp",
    "PermLeCmp",
    "PermLit",
    "PermLtCmp",
    "PermMinus",
    "PermMul",
    "PermSub",
    "PermTExpr",
    "Pointer",
    "PointerTExpr",
    "PredExpr",
    "PredExprFold",
    "PredExprInstance",
    "PredExprUnfold",
    "Predicate",
    "PredicateConstructor",
    "PureClosureCall",
    "PureForall",
    "PureFunction",
    "PureFunctionCall",
    "PureFunctionLit",
    "PureLet",
    "PureMethod",
    "PureMethodCall",
    "PureMethodSubtypeProof",
    "RangeSequence",
    "Receive",
    "Ref",
    "Refute",
    "Rel",
    "Return",
    "SafeMapLookup",
    "SafeReceive",
    "SafeTypeAssertion",
    "Send",
    "SepAnd",
    "SepForall",
    "Seqn",
    "SequenceAppend",
    "SequenceConversion",
    "SequenceDrop",
    "SequenceLit",
    "SequenceTExpr",
    "SequenceTake",
    "SetConversion",
    "SetLit",
    "SetMinus",
    "SetTExpr",
    "ShiftLeft",
    "ShiftRight",
    "SingleAss",
    "Slice",
    "SliceTExpr",
    "SpecImplementationProof",
    "StringLit",
    "StringTExpr",
    "StructLit",
    "StructTExpr",
    "StructUpdate",
    "Sub",
    "Subset",
    "ToInterface",
    "Trigger",
    "Tuple",
    "TupleTExpr",
    "TypeAssertion",
    "TypeOf",
    "Union",
    "UneqCmp",
    "Unfold",
    "Unfolding",
    "Val",
    "Var",
    "While",
    "WildcardPerm"
  )

  private def encodeAny(value: Any, path: String): ExportValue = value match {
    case null => fail(s"cannot export null at $path")
    case n: Node => encodeNode(n)
    case t: TopType => encodeTopType(t)
    case t: Type => encodeType(t)
    case a: Addressability => encodeAddressability(a)
    case k: IntegerKind => encodeIntegerKind(k)
    case b: NumBase => encodeNumBase(b)
    case a: BackendAnnotation => encodeBackendAnnotation(a)
    case t: BuiltInMemberTag => encodeBuiltInTag(t)
    case i: Source.Parser.Info => encodeSourceInfo(i)
    case s: String => str(s)
    case b: Boolean => bool(b)
    case i: Int => int(i)
    case l: Long => int(l)
    case b: BigInt => int(b)
    case o: Option[_] => encodeOption(o, path)
    case m: Map[_, _] => encodeMap(m, path)
    case (left, right) => obj("tag" -> str("Tuple2"), "left" -> encodeAny(left, s"$path._1"), "right" -> encodeAny(right, s"$path._2"))
    case xs: Iterable[_] => arr(xs.toVector.zipWithIndex.map { case (x, i) => encodeAny(x, s"$path[$i]") })
    case other => fail(s"cannot export value of type ${other.getClass.getName} at $path")
  }

  private def encodeOption(value: Option[_], path: String): ExportValue = value match {
    case Some(v) => obj("tag" -> str("Some"), "value" -> encodeAny(v, s"$path.value"))
    case None => obj("tag" -> str("None"))
  }

  private def encodeMap(value: Map[_, _], path: String): ExportValue =
    arr(value.toVector.sortBy { case (k, _) => stableKey(k) }.zipWithIndex.map { case ((k, v), i) =>
      obj(
        "key" -> encodeAny(k, s"$path[$i].key"),
        "value" -> encodeAny(v, s"$path[$i].value")
      )
    })

  private def stableKey(value: Any): String = value match {
    case null => fail("cannot sort null map key")
    case s: String => s
    case i: Int => i.toString
    case l: Long => l.toString
    case b: BigInt => b.toString
    case p: Proxy => p.name
    case t: Type => t.toString
    case n: Node => s"${n.getClass.getSimpleName}:${n.formatted}"
    case other => fail(s"cannot sort map key of type ${other.getClass.getName}")
  }

  private def encodeTopType(t: TopType): ExportValue = t match {
    case typ: Type => encodeType(typ)
    case other => fail(s"cannot export top type of type ${other.getClass.getName}")
  }

  private def encodeType(t: Type): ExportValue = t match {
    case BoolT(addressability) => typeNode("BoolT", addressability)
    case IntT(addressability, kind) => typeNode("IntT", addressability, "kind" -> encodeIntegerKind(kind))
    case Float32T(addressability) => typeNode("Float32T", addressability)
    case Float64T(addressability) => typeNode("Float64T", addressability)
    case StringT(addressability) => typeNode("StringT", addressability)
    case VoidT => obj("tag" -> str("VoidT"))
    case FunctionT(args, res, addressability) =>
      typeNode("FunctionT", addressability, "args" -> arr(args.map(encodeType)), "results" -> arr(res.map(encodeType)))
    case PermissionT(addressability) => typeNode("PermissionT", addressability)
    case SortT => obj("tag" -> str("SortT"))
    case ArrayT(length, elems, addressability) =>
      typeNode("ArrayT", addressability, "length" -> int(length), "elems" -> encodeType(elems))
    case SliceT(elems, addressability) =>
      typeNode("SliceT", addressability, "elems" -> encodeType(elems))
    case MapT(keys, values, addressability) =>
      typeNode("MapT", addressability, "keys" -> encodeType(keys), "values" -> encodeType(values))
    case SequenceT(elem, addressability) =>
      typeNode("SequenceT", addressability, "elem" -> encodeType(elem))
    case SetT(elem, addressability) =>
      typeNode("SetT", addressability, "elem" -> encodeType(elem))
    case MultisetT(elem, addressability) =>
      typeNode("MultisetT", addressability, "elem" -> encodeType(elem))
    case MathMapT(keys, values, addressability) =>
      typeNode("MathMapT", addressability, "keys" -> encodeType(keys), "values" -> encodeType(values))
    case OptionT(elem, addressability) =>
      typeNode("OptionT", addressability, "elem" -> encodeType(elem))
    case DefinedT(name, addressability) =>
      typeNode("DefinedT", addressability, "name" -> str(name))
    case PointerT(elem, addressability) =>
      typeNode("PointerT", addressability, "elem" -> encodeType(elem))
    case TupleT(types, addressability) =>
      typeNode("TupleT", addressability, "types" -> arr(types.map(encodeType)))
    case PredT(args, addressability) =>
      typeNode("PredT", addressability, "args" -> arr(args.map(encodeType)))
    case StructT(fields, ghost, addressability) =>
      typeNode("StructT", addressability, "fields" -> arr(fields.map(encodeNode)), "ghost" -> bool(ghost))
    case InterfaceT(name, addressability) =>
      typeNode("InterfaceT", addressability, "name" -> str(name))
    case DomainT(name, addressability) =>
      typeNode("DomainT", addressability, "name" -> str(name))
    case AdtT(name, definedName, addressability) =>
      typeNode("AdtT", addressability, "name" -> str(name), "definedName" -> str(definedName))
    case AdtClauseT(name, adtT, fields, addressability) =>
      typeNode("AdtClauseT", addressability, "name" -> str(name), "adt" -> encodeType(adtT), "fields" -> arr(fields.map(encodeNode)))
    case ChannelT(elem, addressability) =>
      typeNode("ChannelT", addressability, "elem" -> encodeType(elem))
  }

  private def encodeAddressability(a: Addressability): ExportValue = a match {
    case Addressability.Shared => str("shared")
    case Addressability.Exclusive => str("exclusive")
  }

  private def encodeIntegerKind(k: IntegerKind): ExportValue = k match {
    case UnboundedInteger => obj("tag" -> str("UnboundedInteger"), "name" -> str(k.name))
    case b: BoundedIntegerKind =>
      obj(
        "tag" -> str("BoundedInteger"),
        "name" -> str(b.name),
        "bits" -> int(b.nbits),
        "lower" -> int(b.lower),
        "upper" -> int(b.upper)
      )
    case other => fail(s"cannot export integer kind ${other.getClass.getName}")
  }

  private def encodeNumBase(b: NumBase): ExportValue = b match {
    case Binary => obj("tag" -> str("Binary"), "base" -> int(2))
    case Octal => obj("tag" -> str("Octal"), "base" -> int(8))
    case Decimal => obj("tag" -> str("Decimal"), "base" -> int(10))
    case Hexadecimal => obj("tag" -> str("Hexadecimal"), "base" -> int(16))
    case other => fail(s"cannot export numeric base ${other.getClass.getName}")
  }

  private def encodeBackendAnnotation(a: BackendAnnotation): ExportValue =
    obj("key" -> str(a.key), "values" -> arr(a.values.map(str)))

  private def encodeBuiltInTag(t: BuiltInMemberTag): ExportValue =
    obj(
      "name" -> str(t.name),
      "identifier" -> str(t.identifier),
      "ghost" -> bool(t.ghost)
    )

  private def encodeSourceInfo(info: Source.Parser.Info): ExportValue = info match {
    case Source.Parser.Internal => obj("tag" -> str("Internal"))
    case Source.Parser.Unsourced => fail("cannot export unsourced internal node")
    case Source.Parser.Single(_, origin) =>
      obj(
        "tag" -> str("Single"),
        "origin" -> encodeOrigin(origin)
      )
  }

  private def encodeOrigin(origin: Source.AbstractOrigin): ExportValue = origin match {
    case Source.Origin(pos, tag) =>
      obj("tag" -> str(tag.trim), "position" -> encodePosition(pos))
    case Source.AnnotatedOrigin(inner, annotation) =>
      obj(
        "tag" -> str("AnnotatedOrigin"),
        "origin" -> encodeOrigin(inner),
        "annotation" -> encodeAnnotation(annotation)
      )
  }

  private def encodePosition(pos: AbstractSourcePosition): ExportValue =
    obj(
      "file" -> str(Option(pos.file).map(_.toString).getOrElse("")),
      "start" -> encodeLineColumn(pos.start),
      "end" -> pos.end.map(encodeLineColumn).getOrElse(ExportNull)
    )

  private def encodeLineColumn(pos: HasLineColumn): ExportValue =
    obj("line" -> int(pos.line), "column" -> int(pos.column))

  private def encodeAnnotation(annotation: Source.Annotation): ExportValue = annotation match {
    case Source.OverflowCheckAnnotation => obj("tag" -> str("OverflowCheckAnnotation"))
    case Source.ReceiverNotNilCheckAnnotation => obj("tag" -> str("ReceiverNotNilCheckAnnotation"))
    case Source.ImportPreNotEstablished => obj("tag" -> str("ImportPreNotEstablished"))
    case Source.MainPreNotEstablished => obj("tag" -> str("MainPreNotEstablished"))
    case Source.LoopInvariantNotEstablishedAnnotation => obj("tag" -> str("LoopInvariantNotEstablishedAnnotation"))
    case Source.NoPermissionToRangeExpressionAnnotation() => obj("tag" -> str("NoPermissionToRangeExpressionAnnotation"))
    case Source.InsufficientPermissionToRangeExpressionAnnotation() => obj("tag" -> str("InsufficientPermissionToRangeExpressionAnnotation"))
    case Source.AutoImplProofAnnotation(subT, superT) =>
      obj("tag" -> str("AutoImplProofAnnotation"), "subType" -> str(subT), "superType" -> str(superT))
    case Source.InvalidImplTermMeasureAnnotation() => obj("tag" -> str("InvalidImplTermMeasureAnnotation"))
    case other => fail(s"cannot export source annotation ${other.getClass.getName}")
  }

  private def typeNode(tag: String, addressability: Addressability, fields: (String, ExportValue)*): ExportValue =
    obj((Vector("tag" -> str(tag), "addressability" -> encodeAddressability(addressability)) ++ fields): _*)

  private def nodeObj(tag: String, info: Source.Parser.Info, fields: (String, ExportValue)*): ExportValue =
    obj((Vector("tag" -> str(tag), "source" -> encodeSourceInfo(info)) ++ fields): _*)

  private def obj(fields: (String, ExportValue)*): ExportObject =
    ExportObject(fields.map { case (name, value) => ExportField(name, value) }.toVector)

  private def arr(values: Iterable[ExportValue]): ExportArray =
    ExportArray(values.toVector)

  private def str(value: String): ExportString = ExportString(value)
  private def bool(value: Boolean): ExportBool = ExportBool(value)
  private def int(value: Int): ExportInteger = ExportInteger(value)
  private def int(value: Long): ExportInteger = ExportInteger(value)
  private def int(value: BigInt): ExportInteger = ExportInteger(value)

  private def toJson(value: ExportValue): JValue = value match {
    case ExportObject(fields) => JObject(fields.map(field => JField(field.name, toJson(field.value))).toList)
    case ExportArray(values) => JArray(values.map(toJson).toList)
    case ExportString(value) => JString(value)
    case ExportBool(value) => JBool(value)
    case ExportInteger(value) => JInt(value)
    case ExportNull => JNull
  }
}
