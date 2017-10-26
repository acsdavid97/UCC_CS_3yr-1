#define BUZZER 5
#define FREQ_C 261
#define FREQ_D 294
#define FREQ_E 330
#define FREQ_F 349
#define FREQ_G 392
#define FREQ_A 440
#define FREQ_B 494
// ode of joy eefg gfed ccde edd

void setup() {
  Serial.begin(9600);
  pinMode(BUZZER, OUTPUT);
}

int getFreqFromChar(char c) {
  int freq;
  switch (c) {
    case 'c':
      freq = FREQ_C;
      break;
    case 'd':
      freq = FREQ_D;
      break;
    case 'e':
      freq = FREQ_E;
      break;
    case 'f':
      freq = FREQ_F;
      break;
    case 'g':
      freq = FREQ_G;
      break;
    case 'a':
      freq = FREQ_A;
      break;
    case 'b':
      freq = FREQ_B;
      break;
    case ' ':
      freq = -2;
      break;
    default:
      freq = -1;
  }
  return freq;
}

void loop() {
  static int freq = FREQ_C;
  int pause = 0;
  if (Serial.available() > 0) {
    char freqCode = Serial.read();
    Serial.println(freqCode);
    int newFreq = getFreqFromChar(freqCode);
    if (newFreq > 0) {
      freq = newFreq;
    }
    if (newFreq == -2) {
      pause = 1;
    }

  }
  int periodInMicroSec = ((float)1 / freq) * 1000 * 1000;
  
  int go=50;
  while(go){
    if (!pause) {
      digitalWrite(BUZZER, HIGH);
      delayMicroseconds(periodInMicroSec);
    }
    
    digitalWrite(BUZZER, LOW);
    delayMicroseconds(periodInMicroSec);
    go--;
  }
}
