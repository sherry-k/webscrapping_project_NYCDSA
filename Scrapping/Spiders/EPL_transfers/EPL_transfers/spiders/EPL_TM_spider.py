from scrapy import Spider
from EPL_transfers.items import EplTransfersItem
from scrapy import Request
import re


class EPLSpider(Spider):
    name = 'EPL_TM_spider'
    allowed_urls = ['https://www.transfermarkt.co.uk/']
    start_urls = ['https://www.transfermarkt.co.uk/']
    

    def parse(self, response):
        # List comprehension to generate all the urls for each year
        transferyear_urls = ['https://www.transfermarkt.co.uk/premier-league/transfers/wettbewerb/GB1/plus/?saison_id={}&s_w=&leihe=0&intern=0&intern=1'.format(x) for x in range(1998, 2019)] 
        for url in transferyear_urls:
            yield Request(url=url, callback=self.parse_transfers_page)

    def parse_transfers_page(self, response):
        #this function parses through the page to get info for each club
        Season = response.xpath('//*[@id="main"]/div[10]/div[1]/div[1]/div[1]/text()').extract_first()
        League_Spent = response.xpath('//*[@id="main"]/div[10]/div[2]/div[2]/div[2]/div[6]/span[1]/text()').extract_first()
                 
        #cleaing the scraped data
        Season = ''.join(re.findall('\d+/\d+', Season))
        League_Spent = round(float(''.join(re.findall('\d+', League_Spent)))/10e5,2)

        #looping through each club 
        for i in range (4,24):

            #main xpath for each club
            club = response.xpath('//*[@id="main"]/div[10]/div[1]/div[{}]'.format(i))
        
            #linked xpaths for each item to scrape
            Club = club.xpath('.//div[@class="table-header"]/a')[1].xpath('./text()').extract_first()
            
            Transfer_Expenditure = club.xpath('.//span[@class="transfer-einnahmen-ausgaben redtext"]/text()').extract_first()
            if Transfer_Expenditure == None:
                Transfer_Expenditure = '0.0m'
                Net_Spent = club.xpath('.//div[@class="table-footer footer-border"]/span/text()').extract_first()
                Transfer_Income = club.xpath('.//span[@class="transfer-einnahmen-ausgaben greentext"]/text()').extract_first()
            else:
                Transfer_Income = club.xpath('.//span[@class="transfer-einnahmen-ausgaben greentext"]/text()').extract_first()
                Net_Spent = club.xpath('.//div[@class="table-footer footer-border"]/span/text()').extract_first()
            
            #cleaing the scraped data
            if re.findall('k+', Transfer_Expenditure) != []:
                Transfer_Expenditure =  float(''.join(re.findall('\d+.?\d+', Transfer_Expenditure.strip())))/1000
            else: 
                Transfer_Expenditure = float(''.join(re.findall('\d+.?\d*', Transfer_Expenditure.strip())))
            
            if re.findall('k+', Transfer_Income) != []:
                Transfer_Income =  float(''.join(re.findall('\d+.?\d+', Transfer_Income.strip())))/1000
            else: 
                Transfer_Income = float(''.join(re.findall('\d+.?\d*', Transfer_Income.strip())))

            if re.findall('k+', Net_Spent) != []:
                Net_Spent =  float(''.join(re.findall('-?\d+.?\d+', Net_Spent.strip())))/1000
            else: 
                Net_Spent = float(''.join(re.findall('-?\d+.?\d*', Net_Spent.strip())))

            item = EplTransfersItem()
            item['Season'] = Season 
            item['Club'] = Club
            item['Transfer_Expenditure'] = Transfer_Expenditure
            item['Transfer_Income'] = Transfer_Income
            item['Net_Spent'] = Net_Spent
            item['League_Spent'] = League_Spent
            yield item

