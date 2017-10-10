#define RED 13
#define YELLOW 7
#define GREEN 4
#define RED_DURATION 5000
#define YELLOW_DURATION 100
#define GREEN_DURATION 2500

// the setup function runs once when you press reset or power the board
void setup() {
  for(int i = GREEN; i <= RED; i++) {
    pinMode(i, OUTPUT);
  }  
}



// the loop function runs over and over again forever
void loop() {
  /*
  digitalWrite(RED, HIGH);
  delay(RED_DURATION);
  digitalWrite(RED, LOW);
  digitalWrite(YELLOW, HIGH);
  delay(YELLOW_DURATION);
  digitalWrite(YELLOW, LOW);
  digitalWrite(GREEN, HIGH);
  delay(GREEN_DURATION);
  digitalWrite(GREEN, LOW);
  */

  for(int i = GREEN; i <= RED; i++) {
    turn_LED_on(i, YELLOW_DURATION);
  }
  for(int i = RED; i >= GREEN; i--) {
    turn_LED_on(i, YELLOW_DURATION);
  }
}

void turn_LED_on(int LED, int duration){
  digitalWrite(LED, HIGH);
  delay(duration);
  digitalWrite(LED, LOW);
}

