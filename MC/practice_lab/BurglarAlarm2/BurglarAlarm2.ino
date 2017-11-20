

//Simple Interface Code for the Arduino IR receiver
// Prof J.P.Morrison Nov 13th 2017


#include <IRremote.h>
#include <LiquidCrystal.h>
#include <avr/interrupt.h>
#include <EEPROM.h>
#include <Time.h>
#include <TimeLib.h>

#define  IR_0       0xff6897
#define  IR_1       0xff30cf
#define  IR_2       0xff18e7
#define  IR_3       0xff7a85
#define  IR_4       0xff10ef
#define  IR_5       0xff38c7
#define  IR_6       0xff5aa5
#define  IR_7       0xff42bd
#define  IR_8       0xff4ab5
#define  IR_9       0xff52ad
#define  IR_MINUS   0xffe01f
#define  IR_PLUS    0xffa857
#define  IR_EQ      0xff906f
#define  IR_ON_OFF  0xffa25d
#define  IR_MODE    0xff629d
#define  IR_MUTE    0xffe21d
#define  IR_PLAY    0xffc23d
#define  IR_REW     0xff22dd
#define  IR_FF      0xff02fd
#define  IR_BACK    0xffb04f
#define  IR_100     0xff9867

typedef enum {
  ENTRY_EXIT, DIGITAL, ANALOG, CONTINUOUS
} ALARM_TYPE;

typedef struct {
  ALARM_TYPE alarm_type;
  byte pin;
  byte value;
  byte state;
}ZONE;

const int RECV_PIN = 9;
const int BUZZER = 8;
#define NR_ZONES 4


ZONE zones[NR_ZONES];

IRrecv irrecv(RECV_PIN);
decode_results results;

// function pointer to menu functions.
typedef void (*MenuFunction)();

// initialize the library by associating any needed LCD interface pin
// with the arduino pin number it is connected to
const int rs = 12, en = 11, d4 = 5, d5 = 4, d6 = 3, d7 = 2;
LiquidCrystal lcd(rs, en, d4, d5, d6, d7);

boolean show_time = true;
boolean in_menu = false;

byte menu_index = 0;
const byte MENU_LEN = 2;

byte read_two_digits() {
  byte value = 0;
  for (int i = 0; i < 2;){
    if (irrecv.decode(&results)){
      if (results.value == IR_BACK){
        IR_BACK_CB();
        return 255;
      }
      int key = keyToInt(results.value);
      if (key >= 0 && key < 10) {
        Serial.println(key);
        value = value * 10 + key;
        i++;
      }
      irrecv.resume();
    }
  }
  return value;
}

void set_time_menu(){
  lcd.clear();
  lcd.print("ENTER TIME");

  
  lcd.setCursor(0,1);
  byte hours = read_two_digits();
  if (hours == 255) return;
  if (hours > 24){ 
    lcd.print("ERROR");
    return;
  }
  
  
  lcd.print(hours);
  lcd.print(":");
  byte minutes = read_two_digits();
  if (minutes == 255) return;
  if (minutes > 60){ 
    lcd.print("ERROR");
    return;
  }
 
  
  lcd.print(minutes);
  lcd.print(":");
  byte seconds = read_two_digits();
  if (seconds == 255) return;
  if (seconds > 60){
    lcd.print("ERROR");
    return;
  }
  lcd.print(seconds);
  
  setTime(hours, minutes, seconds, day(), month(), year());
  IR_BACK_CB();
}

void set_date_menu() {
  lcd.clear();
  lcd.print("ENTER DATE");

  lcd.setCursor(0,1);
  byte days = read_two_digits();
  if (days == 255) return;
  if (days > 31){ 
    lcd.print("ERROR");
    return;
  }
  
  lcd.print(days);
  lcd.print(":");
  
  byte months = read_two_digits();
  if (months == 255) return;
  if (months > 12){ 
    lcd.print("ERROR");
    return;
  }
 
  
  lcd.print(months);
  lcd.print(":");
  
  byte years = read_two_digits();
  if (years == 255) return;
  if (years > 37){
    lcd.print("ERROR");
    return;
  }
  lcd.print(years);
  
  setTime(hour(), minute(), second(), days, months, (years+2000));
  IR_BACK_CB();
}


MenuFunction menu_functions[] = {set_time_menu, set_date_menu};

char* menu_names[] = {"SET TIME", "SET DATE"};


int keyToInt(int key_pressed){
  switch(key_pressed){
    case IR_0: return 0;
    case IR_1: return 1;
    case IR_2: return 2;
    case IR_3: return 3;
    case IR_4: return 4;
    case IR_5: return 5;
    case IR_6: return 6;                                         
    case IR_7: return 7;  
    case IR_8: return 8;    
    case IR_9: return 9; 
    case IR_MINUS: return 10;  
    case IR_PLUS: return 11;   
    case IR_EQ: return 12;     
    case IR_ON_OFF: return 13;     
    case IR_MODE: return 14;   
    case IR_MUTE: return 15; 
    case IR_PLAY: return 16;   
    case IR_REW: return 17;  
    case IR_FF: return 18;
    case IR_BACK: return 19;
    case IR_100: return 20; 
    default: return -1;     
  }
}

void updateMenu() {
  lcd.clear();
  lcd.setCursor(0, 0);
  lcd.print("MENU");
  lcd.setCursor(0, 1);
  lcd.print(menu_names[menu_index]);
}

void IR_0_CB(){
  Serial.println("IR_0_CB");
}

void IR_1_CB(){
  Serial.println("IR_1_CB");
}

void IR_2_CB(){
  Serial.println("IR_2_CB");
}

void IR_3_CB(){
  Serial.println("IR_3_CB");
}

void IR_4_CB(){
  Serial.println("IR_4_CB");
}

void IR_5_CB(){
  Serial.println("IR_5_CB");
}

void IR_6_CB(){
  Serial.println("IR_6_CB");
}

void IR_7_CB(){
  Serial.println("IR_7_CB");
}

void IR_8_CB(){
  Serial.println("IR_8_CB");
}

void IR_9_CB(){
  Serial.println("IR_9_CB");
}

void IR_MINUS_CB(){
  Serial.println("IR_MINUS_CB");
}

void IR_PLUS_CB(){
  Serial.println("IR_PLUS_CB");
}

void IR_EQ_CB(){
  Serial.println("IR_EQ_CB");
}

void IR_ON_OFF_CB(){
  Serial.println("IR_ON_OFF_CB");
}

void IR_MODE_CB(){
  Serial.println("IR_MODE_CB");
  // enter menu
  show_time = false;
  in_menu = true;
  
  lcd.clear();
  lcd.setCursor(0, 0);
  menu_index = 0;

  updateMenu();
}

void IR_MUTE_CB(){
  Serial.println("IR_MUTE_CB");
}

void IR_PLAY_CB(){
  Serial.println("IR_PLAY_CB");
  if (in_menu) {
    menu_functions[menu_index]();
  }
}

void IR_REW_CB(){
  Serial.println("IR_REW_CB");
  if (in_menu) {
    menu_index = (menu_index > 0) ? menu_index - 1 : MENU_LEN - 1;
  }
  updateMenu();
}

void IR_FF_CB(){
  Serial.println("IR_FF_CB");
  if (in_menu) {
    menu_index = (menu_index + 1) % MENU_LEN;
  }
  updateMenu();
}

void IR_BACK_CB(){
  Serial.println("IR_BACK_CB");
  lcd.clear();
  show_time = true;
  in_menu = false;
}

void IR_100_CB(){
  Serial.println("IR_100_CB");
}

void printKeyKind(int key_pressed){
  switch(key_pressed){
    case IR_0: Serial.println("0");
                   break;
    case IR_1: Serial.println("1");
                   break;
    case IR_2: Serial.println("2");
                   break;
    case IR_3: Serial.println("3");
                   break;
    case IR_4: Serial.println("4");
                   break;
    case IR_5: Serial.println("5");
                   break;
    case IR_6: Serial.println("6");
                   break;                                         
    case IR_7: Serial.println("7");
                   break;  
    case IR_8: Serial.println("8");
                   break;    
    case IR_9: Serial.println("9");
                   break; 
    case IR_MINUS: Serial.println("MINUS");
                   break;  
    case IR_PLUS: Serial.println("PLUS");
                   break;   
    case IR_EQ: Serial.println("EQ");
                   break;     
    case IR_ON_OFF: Serial.println("ON_OFF");
                   break;     
    case IR_MODE: Serial.println("MODE");
                   break;   
    case IR_MUTE: Serial.println("MUTE");
                   break; 
    case IR_PLAY: Serial.println("PLAY");
                   break;   
    case IR_REW: Serial.println("REW");
                   break;  
    case IR_FF: Serial.println("FF");
                   break;
    case IR_BACK: Serial.println("BACK");
                   break;  
    case IR_100: Serial.println("100+");
                   break;                     
       default: Serial.println(key_pressed);
                   break;    
  }
}

void CBCaller(int keyInt){
  switch (keyInt){
    case 0: IR_0_CB();break;
    case 1: IR_1_CB();break;
    case 2: IR_2_CB();break;
    case 3: IR_3_CB();break;
    case 4: IR_4_CB();break;
    case 5: IR_5_CB();break;
    case 6: IR_6_CB();break;
    case 7: IR_7_CB();break;
    case 8: IR_8_CB();break;
    case 9: IR_9_CB();break;
    case 10: IR_MINUS_CB();break;
    case 11: IR_PLUS_CB();break;
    case 12: IR_EQ_CB();break;
    case 13: IR_ON_OFF_CB();break;
    case 14: IR_MODE_CB();break;
    case 15: IR_MUTE_CB();break;
    case 16: IR_PLAY_CB();break;
    case 17: IR_REW_CB();break;
    case 18: IR_FF_CB();break;
    case 19: IR_BACK_CB();break;
    case 20: IR_100_CB();break;
  }
}

void handle_digital_zone(ZONE *zone) {
//  static byte bState[4] = {LOW, LOW, LOW, LOW};
//  static byte prevBState[4] = {LOW, LOW, LOW, LOW};
//  bState[pin-A0] = digitalRead(pin);
  byte currentState = digitalRead(zone->pin);
  if (currentState && !zone->state){
      Serial.println("pin");
      Serial.println(zone->pin);
      Serial.println("bState pin");
      Serial.println(currentState);
      digitalWrite(BUZZER, HIGH);
      Serial.println("buzzer is high");
      delay(1000);
      digitalWrite(BUZZER, LOW);
      Serial.println("buzzer is low");
  }
  zone->state = currentState;
  
}



void record_event_in_eeprom(ALARM_TYPE at, byte pin){
  
}

byte alarm_type2byte(ALARM_TYPE at) {
  switch(at){
    case ENTRY_EXIT:
      return 0;
    case DIGITAL:
      return 1;
    case ANALOG:
      return 2;
    case CONTINUOUS:
      return 3;
  }
  return 0;
}

ALARM_TYPE byte2alarm_type(byte b) {
  switch(b){
    case 0:
      return ENTRY_EXIT;
    case 1:
      return DIGITAL;
    case 2:
      return ANALOG;
    case 3:
      return CONTINUOUS;
  }
  return ENTRY_EXIT;
}

void record_settings_in_eeprom() {
  //TODO change to zone
  byte packed_alarm_types = 0;
  for(int i = 0; i < NR_ZONES; i++) {
    byte at = alarm_type2byte(zones[i].alarm_type); 
    packed_alarm_types |= at << (2 * i);
  }
  EEPROM.write(0, packed_alarm_types);
  Serial.print("EEPROM: write ");
  Serial.println(packed_alarm_types, BIN);
}

void read_settings_from_eeprom() {
  //TODO change to zone
  byte packed_alarm_types = EEPROM.read(0);
  Serial.print("EEPROM: read ");
  Serial.println(packed_alarm_types, BIN);
  for(int i = 0; i < NR_ZONES; i++) {
    byte packed_at = (packed_alarm_types & (B00000011 << (2 * i))) >> (2 * i);
    ALARM_TYPE at = byte2alarm_type(packed_at);
    zones[i].alarm_type = at;
  }
}

void lcd_print_alarm_types() {
  lcd.setCursor(0, 0);
  
  for(int i = 0; i < NR_ZONES; i++) {
    switch(zones[i].alarm_type) {
      case ENTRY_EXIT:
        lcd.print("E");
        break;
      case DIGITAL:
        lcd.print("D");
        break;
      case ANALOG:
        lcd.print("A");
        break;
      case CONTINUOUS:
        lcd.print("C");
        break;
    }
  }
}

void handle_alarm_zone(ZONE *zone){
  switch (zone->alarm_type) {
    case ENTRY_EXIT:
      break;
    case DIGITAL:
      handle_digital_zone(zone);
      break;
    case ANALOG:
      break;
    case CONTINUOUS:
      break;          
  }
}

void lcd_digits(int digits){
  if(digits < 10){
    lcd.print('0');
  }
  lcd.print(digits);
}

void lcd_time() {
  lcd.setCursor(0, 1);
  lcd_digits(hour());
  lcd.print(":");
  lcd_digits(minute());
  lcd.print(":");
  lcd_digits(second());
  lcd.print("  ");
  lcd_digits(day());
  lcd_digits(month());
  lcd_digits(year() % 100);
  //TODO: remove it in the future
  lcd_print_alarm_types();
}



ISR (TIMER1_COMPA_vect) {
  if (show_time)
    lcd_time();
}

void setup(){
    // set up the LCD's number of columns and rows:
  lcd.begin(16, 2);

  //interrupts
  pinMode(13, OUTPUT);
  pinMode(A0, INPUT);
  pinMode(A1, INPUT);
  pinMode(A2, INPUT);
  pinMode(A3, INPUT);
  pinMode(BUZZER, OUTPUT);
  cli(); //disable global interrupts
  TCCR1A = 0;
  TCCR1B = 0;
  OCR1A = 15625; // set the count corresponding to 1 sec
  TCCR1B |= (1 << WGM12); // Turn on CTC mode
  TCCR1B |= (1 << CS10);
  TCCR1B |= (1 << CS12); // prescale at 1024
  TIMSK1 |= (1 << OCIE1A); //enable CTC interrupt
  
  sei(); // enable global interrupts

  for(int i = 0; i < NR_ZONES; i++){
    // TODO: READ FROM EEPROM PREVIOUS SETTINGS
    zones[i].alarm_type = DIGITAL;
    zones[i].pin = A0 + i;
    zones[i].value = 0;
    zones[i].state = 0;
  }
  
  
  
  Serial.begin(9600); 
  record_settings_in_eeprom();
  //read_settings_from_eeprom();
  
  
  irrecv.enableIRIn(); // Start the receiver
  pinMode(RECV_PIN, INPUT);
}

void loop() {
  if (irrecv.decode(&results)) {
    CBCaller(keyToInt(results.value));
    irrecv.resume();
  }
  for (int i = 0; i < NR_ZONES; i++) {
    handle_alarm_zone(&zones[i]);
  }
}

