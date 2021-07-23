#!/usr/bin/env python
# coding: utf-8

# In[ ]:


from pyspark import SparkConf,SparkContext
from pyspark.streaming import StreamingContext
from pyspark.sql import Row,SQLContext, SparkSession
import sys
import requests

def get_sql_context_instance(spark_context):
	if ('sqlContextSingletonInstance' not in globals()):
		globals()['sqlContextSingletonInstance'] = SQLContext(spark_context)
	return globals()['sqlContextSingletonInstance']

	
def process_rdd(rdd):
	try:
		# Get spark sql singleton context from the current context
		sql_context = get_sql_context_instance(rdd.context)
	
		# convert the RDD to Row RDD
		row_rdd = rdd.map(processToDF)
	
		# create a DF from the Row RDD
		crime_df = sql_context.createDataFrame(row_rdd)
	
		# Register the dataframe as table
		crime_df.registerTempTable("Alerts")
	
		# get the top 10 hashtags from the table using SQL and print them
		crime_counts_df = sql_context.sql("select (*) from Alerts")
		crime_counts_df.show()
	except:
		e = sys.exc_info()[0]
		print("Error: %s" % e)
	

def processToDF(rdd_line):
    return(Row(PrimaryType=rdd_line[0],
               Block=rdd_line[1],
               Description=str(rdd_line[2]),
               LocationDescription=str(rdd_line[3]),
               Arrrest=str(rdd_line[4]),
               Domestic=str(rdd_line[5])))

	
# create spark configuration
conf = SparkConf()
conf.setAppName("ChicagoCrimeAPP")

# create spark context with the above configuration
sc = SparkContext(conf=conf)
sc.setLogLevel("ERROR")

# create the sql context
sqlC = SQLContext(sc)

# create the Streaming Context from the above spark context with interval size 2 seconds
ssc = StreamingContext(sc, 3)

# setting a checkpoint to allow RDD recovery
ssc.checkpoint("Chicago_Crime")

# read data from the port 9089 (in reference to twitter_app.py configuration)
dataStream = ssc.socketTextStream("localhost",9999)

# spliting data
myRDD = dataStream.map(lambda line: line.split(","))

#myDF = sqlC.createDataFrame(myRows)
#myDF.show()
# filter the words to get only hashtags, then map each hashtag to be a pair of (hashtag,1)
#hashtags = words.filter(lambda x: (x, 1))

# adding the count of each hashtag to its last count using updateStateByKey
#tags_totals = hashtags.updateStateByKey(aggregate_tags_count)
#hashtags.pprint()

# do the processing for each RDD generated in each interval
myRDD.foreachRDD(process_rdd)

# start the streaming computation
ssc.start()

# wait for the streaming to finish
ssc.awaitTermination()

