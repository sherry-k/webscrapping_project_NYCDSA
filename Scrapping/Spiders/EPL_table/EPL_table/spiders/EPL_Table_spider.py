from scrapy import Spider
from EPL_table.items import EPLTableItem
from scrapy import Request
import re


class EPLSpider(Spider):
    name = 'EPL_Table_spider'
    allowed_urls = ['https://www.transfermarkt.co.uk/']
    start_urls = ['https://www.transfermarkt.co.uk/']
    

    def parse(self, response):
        # List comprehension to generate all the urls for each page for the league season
        league_position_urls = ['https://www.transfermarkt.co.uk/premier-league/startseite/wettbewerb/GB1/plus/?saison_id={}'.format(x) for x in range(1997, 2019)] 
        for url in league_position_urls:
            yield Request(url=url, callback=self.parse_league_table_page)

    def parse_league_table_page(self, response):
        #this function parses through the page to get information from the final league table for the season
        Season = response.xpath('//*[@id="wettbewerbsstartseite"]/div[2]//div[@class="table-header"]/text()').extract_first()

        Season = ''.join(re.findall('\d+/\d+', Season))
        # looping through each row of the table
        for i in range (1,21):
            #main xpath for each row
            row = response.xpath('//*[@id="wettbewerbsstartseite"]/div[2]//tbody/tr[{}]'.format(i))
            #linked xpaths for each item to scrape
            Position =row.xpath('.//td[@class="rechts hauptlink nowrap"]/text()').extract_first()
            
            Club = row.xpath('.//td[@class="no-border-links hauptlink"]/a/text()').extract_first()
            
            if len(row.xpath('.//td[@class="zentriert"]/text()').extract())> 3:
                Goal_Difference = row.xpath('.//td[@class="zentriert"]/text()').extract()[2]
                Points = row.xpath('.//td[@class="zentriert"]/text()').extract()[3]
            else:
                Goal_Difference = row.xpath('.//td[@class="zentriert"]/text()').extract()[1]
                Points = row.xpath('.//td[@class="zentriert"]/text()').extract()[2]

            Position = int(Position)
            Goal_Difference = int(Goal_Difference)
            Points = int(Points)
            
            # print(Season)
            # print(Club)
            # print(Position)
            # print(Goal_Difference)
            # print(Points)
            item = EPLTableItem()
            item['Season'] = Season 
            item['Club'] = Club
            item['Position'] = Position
            item['Goal_Difference'] = Goal_Difference
            item['Points'] = Points
            yield item


