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
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

class ModelParser(model:String, g:Graph[String,Int] ) {
    var xmlModel : String = model
    var myG : Graph[String,Int] = g
    var activations:Map[String,Double]  = Map()
    var edgeWeights:Map[Int,Double] = Map()
    val ACTIVATION = 0.01
    val ACTIVATIONTHRESHOLD = 0.20
    val INITIALTHRESHOLD = 0.5
    val CTHRESHOLD = 0.001
    val MAXITERATION = 200
    val MAX = 1.0
    val MIN = -1.0
    val DECAYRATE = 0.05
    
     def graphIterator() {
      var V = myG.getVertices
      var itr = V.iterator()
      System.out.println("Vertex List: ")
      while (itr.hasNext()) {
        var myN = itr.next()
        System.out.print("Node name: "+myN+ " ")
        System.out.println("Activation: "+ activations(myN))
         
        var incIter = g.getIncidentEdges(myN).iterator()
        while (incIter.hasNext()) {
          var myEdge = incIter.next()
          System.out.println("Incident edge number: "+ myEdge+" Weight is " + edgeWeights(myEdge) +" Source is "+g.getEndpoints(myEdge).getFirst+" Target is "+ g.getEndpoints(myEdge).getSecond)
          System.out.println("The opposite vertex of"+myN+" in edge"+ myEdge+" is  "+ g.getOpposite(myN,myEdge))
        }
      }   
    }
    
    def getNodes() : ListBuffer[String]  = {
        var rt = new ListBuffer[String]()
        var nStr :String = null
        var V = myG.getVertices
        var itr = V.iterator()
        while (itr.hasNext()) {
            var myN = itr.next()
            nStr = new String(myN)
            rt += nStr
        }
        return rt
    }
    
    def getActivations() : ListBuffer[Double] = {
        var act = new ListBuffer[Double]()
        var V = myG.getVertices
        var itr = V.iterator()
        while (itr.hasNext()) {
            var myN = itr.next()
            var nActivation = activations(myN)
            act += nActivation
        }
        return act
    }
    
    def checkWeight(n1: String, n2: String) : Double = {
        System.out.println("Checking the connection between " + n1 + " and " + n2)
        var eWeight : Double = 0.0
        var V = g.getVertices
        var itr = V.iterator()
        while (itr.hasNext()) {
            var myN = itr.next()
            if (myN == n1) {
                System.out.println("Found source " + myN + " = " + n1)
                var incIter = g.getIncidentEdges(myN).iterator()
                while (incIter.hasNext()) {
                    var myEdge = incIter.next()
                    var nName= g.getEndpoints(myEdge).getSecond
                    if ((nName == n2) && !(n1==n2)) {
                        eWeight=edgeWeights(myEdge)
                        System.out.println("Found " + n2 + " with weight " + eWeight) 
                    }
                }
            }
        }
        return eWeight
    }
    
     def activationCompute() {
      
      var maxChange = 0.0
      var count =0;
      var V = myG.getVertices
      var itr = V.iterator()
      var activationsatT = activations.clone()
      while (/*(maxChange > CTHRESHOLD) &&*/ (count < MAXITERATION)) {
        while (itr.hasNext()) {
          var myNode=itr.next()
          var newA = update(myNode,activationsatT)
          var diffA = Math.abs(newA-activations(myNode))
          if (diffA > maxChange) maxChange = diffA 
          activations(myNode)=newA       
        }
        itr=V.iterator()
        activationsatT = activations.clone()
        count+=1
      }
      activations.foreach(p => println("node= "+p._1 + ", activation= "+p._2))
    }
    
    def update (str:String, actAtT:Map[String,Double]): Double = {
      var cActivation = actAtT(str)
      var nActivation=0.0
      var netFlow = 0.0;
      var itr = g.getIncidentEdges(str).iterator()
      while (itr.hasNext()) {
        var myEdge = itr.next()
        var vt = g.getOpposite(str,myEdge)
        netFlow += edgeWeights(myEdge)*actAtT(vt) 
      }
      
      if (netFlow > 0) {
        nActivation=(cActivation*(1.0-DECAYRATE))+(netFlow*(MAX-cActivation))
        nActivation=Math.max(-1.0,Math.min(1.0, nActivation))
      }
      else {
        nActivation=(cActivation*(1.0-DECAYRATE))+(netFlow*(cActivation-MIN))
        nActivation = Math.min(Math.max(-1.0, nActivation),1.0)
      }
      
      return nActivation
    }
    
    
    def parse() {
        var dbFactory = DocumentBuilderFactory.newInstance()
        var dBuilder = dbFactory.newDocumentBuilder()
        var is = new InputSource(new StringReader(xmlModel)) 
        var doc = dBuilder.parse(is)
        initializeCoherenceGraph()
        doc.getDocumentElement().normalize()
        
        var goalList = doc.getElementsByTagName("goal")
        var myStr : String = null
        for (a <- 0 to goalList.getLength()-1) {
          var nNode = goalList.item(a)
          var eElement = nNode.asInstanceOf[Element]
          myStr = eElement.getElementsByTagName("name").item(0).getTextContent()
          g.addVertex(myStr)
          try {
            var activationLevel = eElement.getElementsByTagName("activation").item(0).getTextContent()
            activations+=(myStr -> activationLevel.toDouble)
          
          }
          catch  {
            case e: Exception => activations+=(myStr -> ACTIVATION)
          }
        }
        var evidenceList = doc.getElementsByTagName("evidence")
       
        for (a <- 0 to evidenceList.getLength()-1) {
          var nNode = evidenceList.item(a)
          var eElement = nNode.asInstanceOf[Element]
          myStr = eElement.getElementsByTagName("name").item(0).getTextContent()
          g.addVertex(myStr)
          try {
            var activationLevel = eElement.getElementsByTagName("activation").item(0).getTextContent()
            activations+=(myStr -> activationLevel.toDouble)
          
          }
          catch  {
            case e: Exception => activations+=(myStr -> ACTIVATION)
          }
        } 
        
        var beliefList = doc.getElementsByTagName("belief")
       
        for (a <- 0 to beliefList.getLength()-1) {
          var nNode = beliefList.item(a)
          var eElement = nNode.asInstanceOf[Element]
          myStr = eElement.getElementsByTagName("name").item(0).getTextContent()
          g.addVertex(myStr)
          try {
            var activationLevel = eElement.getElementsByTagName("activation").item(0).getTextContent()
            activations+=(myStr -> activationLevel.toDouble)
          
          }
          catch  {
            case e: Exception => activations+=(myStr -> ACTIVATION)
          }
        }   
        
         var actionList = doc.getElementsByTagName("action")
       
        for (a <- 0 to actionList.getLength()-1) {
          var nNode = actionList.item(a)
          var eElement = nNode.asInstanceOf[Element]
          myStr = eElement.getElementsByTagName("name").item(0).getTextContent()
          g.addVertex(myStr)
          try {
            var activationLevel = eElement.getElementsByTagName("activation").item(0).getTextContent()
            activations+=(myStr -> activationLevel.toDouble)
          
          }
          catch  {
            case e: Exception => activations+=(myStr -> ACTIVATION)
          }
        } 
         
         var sourceStr : String = null
         var targetStr : String = null
         var weight : Double = 0.0
         var expConstraints = doc.getElementsByTagName("explain")
         for (a <- 0 to expConstraints.getLength()-1) {
           var sNode = expConstraints.item(a)
           var eElement = sNode.asInstanceOf[Element]
           sourceStr = new String(eElement.getElementsByTagName("source").item(0).getTextContent())
           targetStr = new String(eElement.getElementsByTagName("target").item(0).getTextContent())
           weight = eElement.getElementsByTagName("weight").item(0).getTextContent().toDouble
           var edgeCount = g.getEdgeCount()
           g.addEdge(g.getEdgeCount(),sourceStr,targetStr)
           edgeWeights+=(edgeCount -> weight)   
         }
         
         var deduceConstraints = doc.getElementsByTagName("deduce")
         for (a <- 0 to deduceConstraints.getLength()-1) {
           var sNode = deduceConstraints.item(a)
           var eElement = sNode.asInstanceOf[Element]
           sourceStr = new String(eElement.getElementsByTagName("source").item(0).getTextContent())
           targetStr = new String(eElement.getElementsByTagName("target").item(0).getTextContent())
           weight = eElement.getElementsByTagName("weight").item(0).getTextContent().toDouble
           var edgeCount = g.getEdgeCount()
           g.addEdge(g.getEdgeCount(),sourceStr,targetStr)
           edgeWeights+=(edgeCount -> weight)           
         }
         
         var facilitateConstraints = doc.getElementsByTagName("facilitate")
         for (a <- 0 to facilitateConstraints.getLength()-1) {
           var sNode = facilitateConstraints.item(a)
           var eElement = sNode.asInstanceOf[Element]
           sourceStr = new String(eElement.getElementsByTagName("source").item(0).getTextContent())
           targetStr = new String(eElement.getElementsByTagName("target").item(0).getTextContent())
           weight = eElement.getElementsByTagName("weight").item(0).getTextContent().toDouble
           var edgeCount = g.getEdgeCount()
           g.addEdge(g.getEdgeCount(),sourceStr,targetStr)
           edgeWeights+=(edgeCount -> weight)          
         }
         
         var triggerConstraints = doc.getElementsByTagName("trigger")
         for (a <- 0 to triggerConstraints.getLength()-1) {
           var sNode = triggerConstraints.item(a)
           var eElement = sNode.asInstanceOf[Element]
           sourceStr = new String(eElement.getElementsByTagName("source").item(0).getTextContent())
           targetStr = new String(eElement.getElementsByTagName("target").item(0).getTextContent())
           weight = eElement.getElementsByTagName("weight").item(0).getTextContent().toDouble
           var edgeCount = g.getEdgeCount()
           g.addEdge(g.getEdgeCount(),sourceStr,targetStr)
           edgeWeights+=(edgeCount -> weight)           
         }
         
         var inhibitConstraints = doc.getElementsByTagName("inhibit")
         for (a <- 0 to inhibitConstraints.getLength()-1) {
           var sNode = inhibitConstraints.item(a)
           var eElement = sNode.asInstanceOf[Element]
           sourceStr = new String(eElement.getElementsByTagName("source").item(0).getTextContent())
           targetStr = new String(eElement.getElementsByTagName("target").item(0).getTextContent())
           weight = 0-eElement.getElementsByTagName("weight").item(0).getTextContent().toDouble
           var edgeCount = g.getEdgeCount()
           g.addEdge(g.getEdgeCount(),sourceStr,targetStr)
           edgeWeights+=(edgeCount -> weight)           
         }
         
         var incompatibleConstraints = doc.getElementsByTagName("incompatible")
         for (a <- 0 to incompatibleConstraints.getLength()-1) {
           var sNode = incompatibleConstraints.item(a)
           var eElement = sNode.asInstanceOf[Element]
           sourceStr = new String(eElement.getElementsByTagName("source").item(0).getTextContent())
           targetStr = new String(eElement.getElementsByTagName("target").item(0).getTextContent())
           weight = 0-eElement.getElementsByTagName("weight").item(0).getTextContent().toDouble
           var edgeCount = g.getEdgeCount()
           g.addEdge(g.getEdgeCount(),sourceStr,targetStr)
           edgeWeights+=(edgeCount -> weight)          
         }
         
         var contradictConstraints = doc.getElementsByTagName("contradict")
         for (a <- 0 to contradictConstraints.getLength()-1) {
           var sNode = contradictConstraints.item(a)
           var eElement = sNode.asInstanceOf[Element]
           sourceStr = new String(eElement.getElementsByTagName("source").item(0).getTextContent())
           targetStr = new String(eElement.getElementsByTagName("target").item(0).getTextContent())
           weight = 0-eElement.getElementsByTagName("weight").item(0).getTextContent().toDouble
           var edgeCount = g.getEdgeCount()
           g.addEdge(g.getEdgeCount(),sourceStr,targetStr)
           edgeWeights+=(edgeCount -> weight)          
         }
         
         var similarConstraints = doc.getElementsByTagName("similar")
         for (a <- 0 to similarConstraints.getLength()-1) {
           var sNode = similarConstraints.item(a)
           var eElement = sNode.asInstanceOf[Element]
           sourceStr = new String(eElement.getElementsByTagName("source").item(0).getTextContent())
           targetStr = new String(eElement.getElementsByTagName("target").item(0).getTextContent())
           weight = eElement.getElementsByTagName("weight").item(0).getTextContent().toDouble
           var edgeCount = g.getEdgeCount()
           g.addEdge(g.getEdgeCount(),sourceStr,targetStr)
           edgeWeights+=(edgeCount -> weight)           
         }
         
         var competeConstraints = doc.getElementsByTagName("compete")
         for (a <- 0 to competeConstraints.getLength()-1) {
           var sNode = competeConstraints.item(a)
           var eElement = sNode.asInstanceOf[Element]
           sourceStr = new String(eElement.getElementsByTagName("source").item(0).getTextContent())
           targetStr = new String(eElement.getElementsByTagName("target").item(0).getTextContent())
           weight = 0-eElement.getElementsByTagName("weight").item(0).getTextContent().toDouble
           var edgeCount = g.getEdgeCount()
           g.addEdge(g.getEdgeCount(),sourceStr,targetStr)
           edgeWeights+=(edgeCount -> weight)           
         }
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
}