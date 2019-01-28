# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# https://doc.scrapy.org/en/latest/topics/items.html

import scrapy

class EPLTableItem(scrapy.Item):

    Season = scrapy.Field() # season year for league table
    Club = scrapy.Field() # club's name for position in the league table
    Goal_Difference = scrapy.Field() # each club's goal difference for the season
    Points = scrapy.Field() # each club's total point tally for the season
    Position = scrapy.Field() # club's final position in the league table