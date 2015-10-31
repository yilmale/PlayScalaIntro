package models

import edu.uci.ics.jung.algorithms.layout.AbstractLayout
import edu.uci.ics.jung.algorithms.layout.FRLayout2
import edu.uci.ics.jung.algorithms.layout.util.Relaxer
import edu.uci.ics.jung.graph.UndirectedSparseMultigraph
import edu.uci.ics.jung.graph.DirectedSparseMultigraph;
import edu.uci.ics.jung.graph.Graph
import edu.uci.ics.jung.graph.ObservableGraph
import edu.uci.ics.jung.graph.util.Graphs
import edu.uci.ics.jung.visualization.VisualizationViewer
import edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse
import edu.uci.ics.jung.visualization.decorators.ToStringLabeller
import edu.uci.ics.jung.visualization.renderers.Renderer

import javax.xml.parsers.DocumentBuilder
import javax.xml.parsers.DocumentBuilderFactory

import org.w3c.dom.Document
import org.w3c.dom.Element
import org.w3c.dom.Node
import org.w3c.dom.NodeList
import org.xml.sax.InputSource

import scala.io.Source
import java.io._

import java.io.File;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;

import scala.collection.mutable.Map

class ModelParser(model:String, g:Graph[String,Int] ) {
    var xmlModel : String = model
    var myG : Graph[String,Int] = g
    def parse() {
        var dbFactory = DocumentBuilderFactory.newInstance()
        var dBuilder = dbFactory.newDocumentBuilder()
        var is = new InputSource(new StringReader(xmlModel)) 
        var doc = dBuilder.parse(is)
        initializeCoherenceGraph()
        doc.getDocumentElement().normalize()
    }
    
    def initializeCoherenceGraph() {
         if (myG.getVertexCount>0) {
            var V = myG.getVertices
            var vertices=V.toArray()
            for (i <- 0 to myG.getVertexCount-1) {
                myG.removeVertex(vertices(i).asInstanceOf[String])
            }
         } 
        var eC= myG.getEdgeCount()
        if (eC>0) {
            for (i <- 0 to eC-1) {
                myG.removeEdge(i)
            }
        }
    }
    
}

