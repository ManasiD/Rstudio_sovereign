# Variables that can be put on the x and y axes
axis_vars <- c(
  "Spread5" = "E",
  "Spread10" = "F",
  "Gross Debt" = "K",
  "Current Account Balance" = "Q",
  "Fiscal Balance" = "U",
  "Net Debt" = "L",
  "Inflation Annual CPI" = "P",
  "Revenue" = "H",
  "Gross National Savings" = "W",
  "Moody's" = "N"
)
axis_vars2 <- c(
  "A = Country" = "A",
  "Spread5" = "E",
  "Spread10" = "F",
  "H = Revenue" = "H",
  "K = Gross Debt" = "K",
  "L = Net Debt" = "L",
  "P = Inflation Annual CPI" = "P",
  "Q = Current Account Balance" = "Q",
  "U = Fiscal Balance" = "U",
  "W = Gross National Savings" = "W"
  )

HM <- read.csv("HeatMap.csv" ,sep=",")