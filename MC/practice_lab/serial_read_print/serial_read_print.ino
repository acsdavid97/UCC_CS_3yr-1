#define LED 13

boolean ledOn = false;

void setup(){
  pinMode(LED, OUTPUT);
  digitalWrite(LED, LOW);
  Serial.begin(9600);
}

void loop(){
   if (Serial.available() > 0){
      char c = Serial.read();
      if (c == 'a') {
        digitalWrite(LED, (ledOn = !ledOn));
        Serial.print("led status: ");
        Serial.println(ledOn);
      }
      
   }
}
