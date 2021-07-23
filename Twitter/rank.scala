package Deloitte

import org.apache.spark._
import org.apache.spark.rdd.RDD
import org.apache.spark.graphx.{Graph, VertexRDD}
import org.apache.spark.graphx.VertexId



object Main {

    def main( args: Array[ String ] ): Unit = {

        val conf = 
            new SparkConf()
            .setAppName( "RankerTwitter" )
            .setMaster( "local[*]" )
            .set( "spark.driver.host", "localhost" )

        val sc = new SparkContext( conf )

        val file = sc.textFile("twitter_rv.net");

        val edgesRDD: RDD[(VertexId, VertexId)] = 
            file
            .map( line => line.split( "\t" ) )
            .map( line => ( line( 0 ), line( 1 )))

        val graph = Graph.fromEdgeTuples( edgesRDD, 1 )

        // graph.triplets.collect.foreach( println )

        // println( "####" )

        val ranks = 
            graph
            .pageRank( 0.0001 )
            .vertices

        val df = ranks.toDF("ID","Rank").sort("Rank")

        df.show(3)

        println( "####" )

        // Define a reduce operation to compute the highest degree vertex
        def max(a: (VertexId, Int), b: (VertexId, Int)): (VertexId, Int) = {
            if (a._2 > b._2) a else b
        }

        val maxOutDegree: (VertexId, Int) = graph.outDegrees.reduce(max)

        val df = maxOutDegree.toDF("ID","Outdegree")

        df.show()

        // Shortest path between the user following most accounts and any other user
        val sp = graph.shortestPaths.landmarks(Seq("A", "G")).run()

        sp.orderBy("distances").show(1, false)

        /* val identificationMap = 
            file
            .flatMap( line => line.split( "\t" ) )
            .distinct
            .map( line => ( MurmurHash3.stringHash( line.toString ).toLong, line ) )

        identificationMap.foreach( println )

        println( "####" )

        val fullMap = 
            ranks
            .join( identificationMap )

        fullMap.foreach( println )
        */

        sc.stop()
    }
}
