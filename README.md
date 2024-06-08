# Los Angeles Crime Analysis Dashboard
## About the project
This interactive dashboard is designed to provide detailed insights into the safety and crime statistics of various areas within Los Angeles. It allows users to compare crime rates with the LA average, understand the demographics of victims, and assess the overall safety of different neighborhoods.


The safety index provides a quick view of how safe the selected area is relative to others in LA. A higher percentage means higher safety.

## Demo (TO-DO)


## Getting started
1. Clone the repository:
```sh
git clone https://github.com/vasia-korz/crime-dashboard-la
```

2. Enter the directory and create `data` directory:
```sh
cd crime-dashboard-la
mkdir data
```
3. Download the [dataset](https://data.lacity.org/Public-Safety/Crime-Data-from-2020-to-Present/2nrs-mtv8/about_data) and put it into `data` directory. You should have `dataset_extended.csv` file in that folder.

4. Run preprocessing script:
- Windows
```sh
python preprocessing.py
```
- macOS/Linux
```sh
python3 preprocessing.py
```

5. Launch the application
- From RStudio: click on the button "Run app" with green arrow.
- From CLI:
```sh
bash run.sh
```



## Source
The visualization would not be possible without the collected data. The details, with the dataset itself, you can find on the [Los Angeles - Open Data Portal](https://data.lacity.org/Public-Safety/Crime-Data-from-2020-to-Present/2nrs-mtv8/about_data). Design decisions forced us to apply several preprocessing steps, which you can find in the [preprocessing.py](preprocessing.py) script.