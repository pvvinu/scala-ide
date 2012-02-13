package scala.tools.eclipse.semantichighlighting.classifier

import scala.tools.nsc.util.BatchSourceFile
import org.junit.Before
import scala.tools.eclipse.testsetup.TestProjectSetup

class AbstractSymbolClassifierTest extends TestProjectSetup("semantic-highlighting") {

  protected def checkSymbolClassification(source: String, locationTemplate: String, regionTagToSymbolType: Map[String, SymbolType]) {
    val expectedRegionToSymbolNameMap: Map[Region, String] = RegionParser.getRegions(locationTemplate)
    val expectedRegionsAndSymbols: List[(Region, SymbolType)] =
      expectedRegionToSymbolNameMap.mapValues(regionTagToSymbolType).toList sortBy regionOffset
    val actualRegionsAndSymbols: List[(Region, SymbolType)] =
      classifySymbols(source, expectedRegionToSymbolNameMap.keySet) sortBy regionOffset

    if (expectedRegionsAndSymbols != actualRegionsAndSymbols) {
      val sb = new StringBuffer
      def displayRegions(regionToSymbolInfoMap: List[(Region, SymbolType)]) = {
        regionToSymbolInfoMap.toList.sortBy(regionOffset) map {
          case (region, symbolType) =>
            "  " + region + " '" + region.of(source) + "' " + symbolType
        } mkString "\n"
      }
      sb.append("Actual != Expected.\n")
      sb.append("Expected:\n")
      sb.append(displayRegions(expectedRegionsAndSymbols))
      sb.append("\nActual:\n")
      sb.append(displayRegions(actualRegionsAndSymbols))
      throw new AssertionError(sb.toString)
    }
  }

  private def classifySymbols(source: String, restrictToRegions: Set[Region]): List[(Region, SymbolType)] = {
    val sourceFile = new BatchSourceFile("", source)
    project.withPresentationCompiler { compiler =>
      val symbolInfos: List[SymbolInfo] = SymbolClassifier.classifySymbols(sourceFile, compiler, useSyntacticHints = true)
      for {
        SymbolInfo(symbolType, regions, deprecated) <- symbolInfos
        region <- regions
        if restrictToRegions exists region.intersects
      } yield (region, symbolType)
    }(orElse = Nil)
  }.distinct sortBy regionOffset

  private def regionOffset(regionAndSymbolType: (Region, SymbolType)) = regionAndSymbolType._1.offset

}