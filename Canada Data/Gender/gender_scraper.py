import requests
import bs4
import unidecode
import csv

with open("gender_out.csv", "w") as f:
	wri = csv.writer(f)
	wri.writerow(["Name", "Party", "Riding", "Province", "Status", "ElectionNum"])
	for i in xrange(13, 43):
		url = "http://www.lop.parl.gc.ca/About/Parliament/FederalRidingsHistory/hfer.asp?Language=E&Search=WomenCandidate&Election=" + str(i)

		page = requests.get(url).text
		parser = bs4.BeautifulSoup(page, "lxml")
		m_c = parser.find("div", {"id": "MainContent"}).find("table").find_all("tr")
		j=0
		for r in m_c:
			if j == 0:
				j = j + 1
				continue
			td = r.find_all("td")
			if len(td) != 4:
				continue

			name, party, riding, status = [x.text for x in td]
			name = name.encode("iso-8859-1")
			riding = riding.encode("iso-8859-1")
			riding, province = riding.rsplit(", ", 1)
			party = party.encode("iso-8859-1")
			elected = 0
			print status
			if status == "Defeated":
				elected = 0
			else:
				elected = 1

			print name, party, riding, province, elected, i
			wri.writerow([name, party, riding, province, elected, i])
