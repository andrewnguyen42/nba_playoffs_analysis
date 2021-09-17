from basketball_reference_web_scraper import client
from basketball_reference_web_scraper.data import OutputType

years = range(1998, 2020)

def get_data(year):
  client.season_schedule(
    season_end_year=year, 
    output_type=OutputType.CSV, 
    output_file_path=f'./data/%s_season.csv'%(year)
  )

list(map(get_data, years))

