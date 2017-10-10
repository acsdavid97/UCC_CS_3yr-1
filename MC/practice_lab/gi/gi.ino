#define SWITCH 8
#define LED 6
#define DEBOUNCE_TIME 5

boolean prevBState = LOW;
boolean bState = LOW;
int ledBrigthness = 0;
int ledChange = 20;


boolean debounceRead(byte input){
   if(digitalRead(input)){
      delay(DEBOUNCE_TIME);
      return digitalRead(input);
   }
   return LOW;
}

void setup(){
  pinMode(SWITCH, INPUT);
  pinMode(LED, OUTPUT);
  analogWrite(LED, 0);
  Serial.begin(9600);
}

void loop(){
   bState = debounceRead(SWITCH);
   if (!bState && prevBState){
      
      Serial.print("led status: ");
      Serial.println(ledBrigthness);
      ledBrigthness += ledChange;
      analogWrite(LED, ledBrigthness);
      if (ledBrigthness + ledChange <=0 || ledBrigthness + ledChange >= 255) {
        ledChange = -ledChange;
      }
      
   }
   prevBState = bState;
}
