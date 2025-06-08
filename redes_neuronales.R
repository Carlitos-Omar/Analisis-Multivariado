
# Escalar los datos (solo los predictores, no Outcome)
diabetes$Pregnancies = scale(diabetes$Pregnancies)
diabetes$Glucose = scale(diabetes$Glucose)
diabetes$BloodPressure = scale(diabetes$BloodPressure)
diabetes$SkinThickness = scale(diabetes$SkinThickness)
diabetes$Insulin = scale(diabetes$Insulin)
diabetes$BMI = scale(diabetes$BMI)
diabetes$DiabetesPedigreeFunction = scale(diabetes$DiabetesPedigreeFunction)
diabetes$Age = scale(diabetes$Age)

# Creamos nuestro modelo
modelo = nnet(Outcome ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age,
              data = diabetes,
              size = 9, maxit = 500, decay = 0.01, linout = FALSE)

print(modelo)

# Hacer predicciones
predicciones = predict(modelo, diabetes, type = "raw")
print(predicciones)

# Comparar predicciones con valores reales
tabla = table(Predicho = predicciones, Real = diabetes$Outcome)
print(tabla)

# Calcular precisión
Precision = sum(diag(tabla)) / sum(tabla)
cat("Precisión del modelo:", round(Precision * 100, 2), "%\n")
