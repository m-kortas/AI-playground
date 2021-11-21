import pandas as pd
import numpy as np
from bokeh.io import output_notebook, show, output_file, curdoc
from bokeh.plotting import figure
from bokeh.models import GeoJSONDataSource, LinearColorMapper, ColorBar,Title,Slider, HoverTool
from bokeh.palettes import brewer
from bokeh.layouts import widgetbox, row, column
from bokeh.plotting import figure, output_file, save
import json
import geopandas as gpd

merged = pd.read_csv('../data/data.csv') # your data, the municipality column is called "city"
merged = merged[['city','sum']]

geo_url='https://opendata.arcgis.com/datasets/620c2ab925f64ed5979d251ba7753b7f_0.geojson' 
geo_data = gpd.read_file(geo_url)
geo_data = geo_data[['Gemeentenaam','Provincie','geometry']]
geo_data.columns = ['city', 'province', 'geometry']

data = pd.merge(geo_data, merged, how='left', on= ['city']).dropna(subset=['geometry'])

merged_json = json.loads(data.to_json())
json_data = json.dumps(merged_json)

def plot_map(column, title, low, high, text):
    geosource = GeoJSONDataSource(geojson = json_data)
    palette = brewer['YlOrRd'][4]
    palette = palette[::-1]
    hover = HoverTool(tooltips = [ ('city','@city'),(column, '@'+str(column))])
    tick_labels = {high: str(high)+'+'}
    color_mapper = LinearColorMapper(palette = palette, low = low, high = high, nan_color = '#ffffff')
    color_bar = ColorBar(color_mapper=color_mapper, label_standoff=8,width = 500, height = 20,
    border_line_color=None,location = (0,0), orientation = 'horizontal', major_label_overrides = tick_labels)
    p = figure(title = title, plot_height = 500 , plot_width = 350, toolbar_location = None, tools = [hover])
    p.xgrid.grid_line_color = None
    p.ygrid.grid_line_color = None
    p.patches('xs','ys', source = geosource,fill_color = {'field' :column, 'transform' : color_mapper},
              line_color = 'black', line_width = 0.25, fill_alpha = 1)
    p.add_layout(color_bar, 'below')
    p.add_layout(Title(text = text, align="center"), "below")
    output_notebook()
    show(p)    
    
for col in ['sum']:  #choose for which columns
    output_file(col+".html")
    plot = plot_map(column=col,
         title="CO2 emissions per municipality: " + col,
         low=6,
         high=1000000,
         text='')
    plot
    save(plot)    
