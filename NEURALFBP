import pandas as pd
from neuralprophet import NeuralProphet
import pandas_datareader as pdr
from datetime import datetime
import matplotlib.pyplot as plt
#THE ALGORITM WAS REPEATED FOR ALL CASES CONSIDERED
data  = pd.read_csv('../examples/exam.csv', ';')
print(data.tail(44))
prcp_data = data.rename(columns={'ds': 'ds', 'y': 'y'})[['ds', 'y']]
model = NeuralProphet()
metrics = model.fit(prcp_data, validate_each_epoch=True, 
                    valid_p=0.2, freq='D', 
                    plot_live_loss=True, epochs=10)
future = model.make_future_dataframe(prcp_data, periods=180)
forecast = model.predict(future)
forecasts_plot = model.plot(forecast)
fig_comp = model.plot_components(forecast)

start = datetime(2020, 10, 13)
end = datetime(2021, 1, 11)
COVID_data = pdr.get_data_fred('COVID', start, end)
plt.figure(figsize=(10, 7))
plt.plot(COVID_data)
plt.title('COVID')
COVID_data = COVID_data.reset_index().rename(columns={'DATE': 'ds', 'COVID': 'y'}) # the usual preprocessing routine
model = NeuralProphet(n_changepoints=100,
                      trend_reg=0.05,
                      yearly_seasonality=False,
                      weekly_seasonality=False,
                      daily_seasonality=False)
metrics = model.fit(COVID_data, validate_each_epoch=True, 
                    valid_p=0.2, freq='D', 
                    plot_live_loss=True, 
                    epochs=100)
def plot_forecast(model, data, periods, historic_pred=True, highlight_steps_ahead=None):
  
    """ plot_forecast function - generates and plots the forecasts for a NeuralProphet model
    - model -> a trained NeuralProphet model
    - data -> the dataframe used for training
    - periods -> the number of periods to forecast
    - historic_pred -> a flag indicating whether or not to plot the model's predictions on historic data
    - highlight_steps_ahead -> the number of steps ahead of the forecast line to highlight, used for autoregressive models only"""
    
    future = model.make_future_dataframe(data, 
                                         periods=periods, 
                                         n_historic_predictions=historic_pred)
    forecast = model.predict(future)
    
    if highlight_steps_ahead is not None:
        model = model.highlight_nth_step_ahead_of_each_forecast(highlight_steps_ahead)
        model.plot_last_forecast(forecast)
    else:    
        model.plot(forecast)

plot_forecast(model, COVID_data, periods=20)
model = NeuralProphet(n_changepoints=100,
                      trend_reg=0.5,
                      yearly_seasonality=False,
                      weekly_seasonality=True,
                      daily_seasonality=False)
metrics = model.fit(COVID_data, validate_each_epoch=True, 
                    valid_p=0.2, freq='D', 
                    plot_live_loss=True, 
                    epochs=100)
plot_forecast(model, COVID_data, periods=180)
model = NeuralProphet(
    n_forecasts=20,
    n_lags=20,
    n_changepoints=100,
    yearly_seasonality=True,
    weekly_seasonality=False,
    daily_seasonality=False,
    batch_size=64,
    epochs=100,
    learning_rate=1.0,
)
model.fit(COVID_data, 
          freq='D',
          valid_p=0.2,
          epochs=100)
plot_forecast(model, COVID_data, periods=180)