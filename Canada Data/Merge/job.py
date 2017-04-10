import sys
import operator
import re
import traceback
import math
import pprint
import time
import datetime
import csv 
startTime = time.time()

print "Load all elections, sort to fit by-elections."
# Sort the rows so that by-elections are processed at the proper time so incumbency is set properly for by-elected members
lines = [x for x in open("input.txt","r").read().split("\n") if len(x)]
skipLines = lines[1:]
sortedLines = sorted(skipLines, key=lambda l: float(l.rsplit("\t",1)[1]))
lines = [lines[0]] + sortedLines

print "Beginning to iterate through election data."
jobSet = {}
i = 0
for line in lines:
	if len(line.strip())==0:
		break

	preID, province, riding, date, name, party, job, votes, percent, elected, electionnum = line.split("\t")
	if "/" in job:
		jobs = job.split("/")
	elif ", " in job:
		jobs = job.split(", ")
	elif " and " in job:
		jobs = job.split(" and ")
	elif " - " in job:
		jobs = job.split(" - ")
	else:
		jobs = [job]

	jobs = [x.strip() for x in jobs]

	for j in jobs:
		j = j.lower()
		if not len(j):
			continue
		if j in jobSet:
			jobSet[j] = jobSet[j] + 1
		else:
			jobSet[j] = 1
		i=i+1




lawJobs = ["lawyer", "barrister", "notary", "barrister-at-law", "solicitor", "notary public", "law professor", "barrister & solicitor", "judge", "law teacher", "barrister-solicitor", "law", "professor of law", "retired judge", "lawyer (retired)", "lecturer in law"]
businessJobs = ["merchant", "businessman", "accountant", "economist", "business owner", "business person", "chartered accountant", "businesswoman", "entrepreneur", "business manager", "business executive", "general merchant", "businessperson", "small businessman", "chief executive officer", "small business"]
polJobs = ["parliamentarian", "member of parliament", "politician", "representative", "mayor", "municipal councillor", "superintendent", "cabinet minister", "labour representative", "political organizer", "city councillor", "community organizer", "alderman", "political advisor", "parliamentarian (provincial)", "privy council", "political party leader", "leader of political party"]

q = 0
sorted_jobs = sorted(jobSet.items(), key=operator.itemgetter(1))
sorted_jobs.reverse()
for k, v in sorted_jobs:
	if v>5:
		print k, v

	if k.strip() in lawJobs:
		q=q+v
	elif k.strip() in businessJobs:
		q=q+v
	elif k.strip() in polJobs:
		q=q+v

print "total jobs", i
print "good jobs", q
