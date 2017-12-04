//save the password in EEPROM

//Simple Interface Code for the Arduino IR receiver
// Prof J.P.Morrison Nov 13th 2017


#include <IRremote.h>
#include <LiquidCrystal.h>
#include <avr/interrupt.h>
#include <EEPROM.h>
#include <Time.h>
#include <TimeLib.h>

#include "zone.h"
#include "event.h"

//IR codes
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

// generic lengths
#define NR_ZONES 4
#define PASSWORD_LEN 4

//EEPROM addresses
#define ALARM_EVENT_ADDR 4
#define USER_PASSWORD_ADDR 0
#define ENGINEER_PASSWORD_ADDR 2
#define ZONES_ADDR 6
#define MINIM_EVENT_OFFSET 26
#define EVENT_SIZE 5

//pins
#define RECV_PIN 9
#define BUZZER 8
#define WARNING_LED 7

//array of alarm zones
ZONE zones[NR_ZONES];

IRrecv irrecv(RECV_PIN);
decode_results results;


// function pointer to menu functions.
typedef void (*MenuFunction)();

// initialize the library by associating any needed LCD interface pin
// with the arduino pin number it is connected to
const int rs = 12, en = 11, d4 = 5, d5 = 4, d6 = 3, d7 = 2;
LiquidCrystal lcd(rs, en, d4, d5, d6, d7);

void set_time_menu();
void set_date_menu();
void set_user_password_menu();
void set_engineer_password_menu();
void set_zone_menu();

//menu functions and their corresponding names.
MenuFunction menu_functions[] = {set_time_menu, set_date_menu, set_user_password_menu, set_engineer_password_menu, set_zone_menu};
char* menu_names[] = {"SET TIME", "SET DATE", "SET USR PASSWORD", "SET ENG PASSWORD", "SET ZONES"};

byte menu_index = 0;
const byte MENU_LEN = sizeof(menu_functions)/sizeof(MenuFunction);

//global booleans, to control menus and alarms and LCD
boolean show_time = true;
boolean in_menu = false;
boolean alarm_rang = false;

//passwords
int user_password = 0;
int engineer_password = 0;

int alarm_active_time = 0;

int alarm_event_offset = MINIM_EVENT_OFFSET;


//callback functions for IR buttons
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

// Turn alarm ON
void IR_ON_OFF_CB(){
  Serial.println("IR_ON_OFF_CB");
  //activate alarm
  lcd.clear();
  lcd.setCursor(0,0);
  lcd.print("ACTIVE ALARM");
  lcd.setCursor(0,1);
  show_time = false;
  int password = read_password();
  if (password == user_password) {
    alarm_active_time = millis()/1000;
    for(byte i = 0; i < NR_ZONES; i++) {
      zones[i].active = true;
    }
  }else{
    lcd.setCursor(0,0);
    lcd.print("WRONG PASSWORD");
    delay(1000);
  }
  lcd.clear();
  show_time = true;
}

// enter menus
void IR_MODE_CB(){
  Serial.println("IR_MODE_CB");
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

// call the selected menu
void IR_PLAY_CB(){
  Serial.println("IR_PLAY_CB");
  if (in_menu) {
    menu_functions[menu_index]();
  }
}

//previous menu
void IR_REW_CB(){
  Serial.println("IR_REW_CB");
  if (in_menu) {
    menu_index = (menu_index > 0) ? menu_index - 1 : MENU_LEN - 1;
  }
  updateMenu();
}

//next menu
void IR_FF_CB(){
  Serial.println("IR_FF_CB");
  if (in_menu) {
    menu_index = (menu_index + 1) % MENU_LEN;
  }
  updateMenu();
}

//return from the menu to the home screen
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

// waits until a digit is read from the IR receiver.
// If back button is pressed, exits menu and returns 255;
byte read_digit() {
  byte i = 0;
  for (i=0; i<1;) {
    if (irrecv.decode(&results)){
      if (results.value == IR_BACK){
        IR_BACK_CB();
        irrecv.resume();
        return 255;
      }
      int key = keyToInt(results.value);
      if (key >= 0 && key < 10) {
        Serial.print("key read:");
        Serial.println(key);
        i++;
        irrecv.resume();
        return key;
      }
      irrecv.resume();
    }
  }
}

// reads two digits, behaviour is similar to read_digit()
byte read_two_digits() {
  byte fst = read_digit();
  if (fst == 255) {
    return 255;
  }
  lcd.print(fst);
  byte snd = read_digit();
  if (snd == 255) {
    return 255;
  }
  lcd.print(snd);
  return 10 * fst + snd;
}

// menu for setting the current time
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
  
  //lcd.print(hours);
  lcd.print(":");
  byte minutes = read_two_digits();
  if (minutes == 255) return;
  if (minutes > 60){ 
    lcd.print("ERROR");
    return;
  }
  
  //lcd.print(minutes);
  lcd.print(":");
  byte seconds = read_two_digits();
  if (seconds == 255) return;
  if (seconds > 60){
    lcd.print("ERROR");
    return;
  }
  //lcd.print(seconds);
  
  setTime(hours, minutes, seconds, day(), month(), year());
  IR_BACK_CB();
}

// menu for setting the current date
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
  lcd.print(":");
  
  byte months = read_two_digits();
  if (months == 255) return;
  if (months > 12){ 
    lcd.print("ERROR");
    return;
  }
  lcd.print(":");
  
  byte years = read_two_digits();
  if (years == 255) return;
  if (years > 37){
    lcd.print("ERROR");
    return;
  }
  
  setTime(hour(), minute(), second(), days, months, (years+2000));
  IR_BACK_CB();
}

//reads a 4 digit password from the IR receiver, if password is successfully
//read, then it is returned, otherwise -1 is returned
int read_password() {
  int password = 0;
  lcd.setCursor(0, 1);
  for(byte i = 0; i < PASSWORD_LEN; i++) {
    byte digit = read_digit();
    lcd.print("*");
    if (digit == 255) {
      return -1;
    }
    password = 10 * password + digit;
  }
  delay(500);
  return password;
}

// function to set a new password
// reads a password, and changes the password passed as a pointer
boolean verify_set_password(int *password){
   int read_pssw = read_password();
   if (read_pssw == *password) {
    lcd.clear();
    lcd.print("NEW PASSWORD");
    int new_pass = read_password();
    if (new_pass == -1) {
      return false;
    }
    *password = new_pass;
    lcd.clear();
    lcd.print("SUCCESS");
    return true;
  }else {
    lcd.setCursor(0, 0);
    lcd.print("INCORRECT PASSWORD");
    return false;
  }
}

//menu to change a password, choice determines if ENGINEER or USER password
void set_password_menu(byte choice){
  lcd.clear();
  lcd.setCursor(0, 0);
  lcd.print("CURRENT PASSWORD");
  lcd.setCursor(0, 0);
  boolean verify = false;
  if (choice){
    verify = verify_set_password(&engineer_password);
    if (verify){
      record_password_in_eeprom(engineer_password, choice);
    }
  }
  else{
    verify = verify_set_password(&user_password);
    if (verify){
      record_password_in_eeprom(user_password, choice);
    }  
  } 
  delay(1500);
  IR_BACK_CB();
  
}

void set_user_password_menu() {
  set_password_menu(0);
}

void set_engineer_password_menu() {
  set_password_menu(1);
}

// set the zone type and the zone parameters
void handle_set_zone(byte choice){
  // choose the zone type
  lcd.clear();
  lcd.setCursor(0, 0);
  lcd.print("ENTER TYPE");
  lcd.setCursor(0, 1);
  lcd.print("EE-1 D-2 A-3 C-4");
  byte zone = read_digit();
  if (zone < 1 || zone > 4){
    lcd.clear();
    lcd.print("ERROR");
    delay(1000);
    IR_BACK_CB();
    return;
  }  
  ALARM_TYPE at = byte2alarm_type(zone - 1);

  //parameters for each zone type.
  lcd.clear();
  lcd.setCursor(0, 0);
  byte value = 0;
  int threshold = 0;
  switch (at){
    case ENTRY_EXIT:
      lcd.print("ENTER E/E TIME");
      lcd.setCursor(0,1);
      value = read_two_digits();
      if (value == 255){
        lcd.print("ERROR");
        delay(1000);
        return;
      }
      zones[choice].alarm_type = at;
      zones[choice].value = value;
      zones[choice].state = 0;
      record_settings_in_eeprom();
      break;         
    case DIGITAL:
      lcd.print("ENTER ACTIVE TYPE");
      lcd.setCursor(0, 1);
      lcd.print("0->1(1) 1->0(0)");
      value = read_digit();
      if (value == 0 || value == 1){
        zones[choice].alarm_type = at;
        zones[choice].value = value;
        zones[choice].state = 0;
        record_settings_in_eeprom();       
      }
      break;
    case ANALOG:
      lcd.print("ENTER THRESHOLD");
      lcd.setCursor(0,1);
      for (byte i = 0; i < 4; i++){
        byte digit = read_digit();
        lcd.print(digit);
        if (digit == 255){
          break;
        }
        threshold = threshold * 10 + digit;
      }
      zones[choice].alarm_type = at;
      zones[choice].value = threshold;
      zones[choice].state = 0;
      record_settings_in_eeprom();
      break;
    case CONTINUOUS:
      zones[choice].alarm_type = at;
      zones[choice].value = 0;
      zones[choice].state = 0;
      record_settings_in_eeprom();      
  }
  IR_BACK_CB();
}

//menu for changing zone types and their corresponding parameters
void set_zone_menu() {
  // check for engineer password
  lcd.clear();
  lcd.print("ENG PASSWORD");
  int password = read_password();
  if (password != engineer_password){
    lcd.clear();
    lcd.print("WRONG PASSWORD");
    delay(1000);
    IR_BACK_CB();
    return;
  }

  //select zone number
  lcd.clear();
  lcd.print("SELECT ZONE");
  lcd.setCursor(0, 1);
  lcd.print("BETWEEN 1-");
  lcd.print(NR_ZONES);
  byte choice = read_digit();
  if (choice < 1 || choice > NR_ZONES){
    lcd.clear();
    lcd.print("ERROR");
    delay(1000);
    IR_BACK_CB();
    return;
  }
  handle_set_zone(choice - 1);
    
}

//update the menu for the current menu option
void updateMenu() {
  lcd.clear();
  lcd.setCursor(0, 0);
  lcd.print("MENU");
  lcd.setCursor(0, 1);
  lcd.print(menu_names[menu_index]);
}

// start the alarm when one of the zones is triggered
void ring_the_alarm(ZONE *zone) {
  alarm_rang = true;
  show_time = false;
  digitalWrite(BUZZER, HIGH);
  record_event_in_eeprom(zone);
  int password;
  do {
    lcd.clear();
    lcd.print("ENTER PASSWORD");
    lcd.setCursor(0, 1);
    password = read_password();
    Serial.print("password:");
    Serial.println(password);
  }while(password != user_password);
  digitalWrite(BUZZER, LOW);
  show_time = true;
  lcd.clear();
}

// check if entry exit zone condition is met: 
// door opened, password is not entered in zone->value seconds
void handle_entry_exit_zone(ZONE *zone){
  // if zone is activated and exit time is elapsed
  if (zone->active && 
    alarm_active_time + zone->value < millis()/1000 ) {
    byte currentState = digitalRead(zone -> pin);

    // if the zone is triggered
    if (currentState && !zone->state){
      int password;
      show_time = false;
      zone->time_triggered = millis()/1000;
      zone->triggered = true;
      do{
        lcd.clear();
        lcd.setCursor(0, 0);
        lcd.print("ENTER PASSWORD");
        lcd.setCursor(0, 1);
        password = read_password();
        if (password == user_password) {
        }else {
          lcd.clear();
          lcd.print("WRONG PASSWORD");
          delay(1000);
        }
        //turn off is password is entered
      }while(user_password != password);
      
      lcd.clear();
      digitalWrite(WARNING_LED, LOW);
      show_time = true; 
      zone->active = false;
      zone->triggered = false;
      zone->time_triggered = 0;
      digitalWrite(BUZZER, LOW);
      show_time = true;
      lcd.clear();
    }
    zone->state = currentState;
  }
}

// check if digital zone trigger condition is met: HIGH -> LOW or LOW -> HIGH
void handle_digital_zone(ZONE *zone) {
  byte currentState = digitalRead(zone->pin);
  // check if HIGH-> LOW or LOW->HIGH
  if (zone->value) {
    if (currentState && !zone->state){
      ring_the_alarm(zone);
    }
  }else {
    if (!currentState && zone->state){
      ring_the_alarm(zone);
    }
  }
  zone->state = currentState;
  
}

// check if threshold on pin is higher then the value specified
void handle_analog_zone(ZONE *zone) {
  
  int value = analogRead(zone->pin);
  if (value >= zone->value && zone->state < zone->value) {
    ring_the_alarm(zone);
  }
  zone->state = value;
}

// save the new offset of the events in the EEPROM
void record_alarm_event_offset() {
  Serial.print("alarm offset written");
  Serial.println(alarm_event_offset);
  EEPROM.write(ALARM_EVENT_ADDR + 0, highByte(alarm_event_offset));
  EEPROM.write(ALARM_EVENT_ADDR + 1, lowByte(alarm_event_offset));
}


//read the alarm event offset from the EEPROM
void read_alarm_event_offset() {
  byte hi = EEPROM.read(ALARM_EVENT_ADDR + 0);
  byte lo = EEPROM.read(ALARM_EVENT_ADDR + 1);
  alarm_event_offset = word(hi, lo);
  Serial.print("alarm offset read");
  Serial.println(alarm_event_offset);
}

//write an alarm event to the EEPROM sibling function with read_event_from_eeprom();
void record_event_in_eeprom(ZONE *zone){
  int offset = alarm_event_offset;
  time_t t = now();
  Serial.print("time when recorded");
  Serial.println(t);
  unsigned int fst = t >> 16;
  unsigned int snd = (unsigned int) t;
  EEPROM.write(offset + 0, highByte(fst));
  EEPROM.write(offset + 1, lowByte(fst));
  EEPROM.write(offset + 2, highByte(snd));
  EEPROM.write(offset + 3, lowByte(snd));
  byte packed_pin_zone = zone->pin;
  byte at = alarm_type2byte(zone->alarm_type);
  packed_pin_zone |= (at << 6);
  EEPROM.write(offset + 4, packed_pin_zone);
  alarm_event_offset += EVENT_SIZE;
  if (alarm_event_offset > 512 - EVENT_SIZE) {
    alarm_event_offset = MINIM_EVENT_OFFSET;
  }
  record_alarm_event_offset();
}

//read an event from the EEPROM sibling function with record_event_in_eeprom();
EVENT read_event_from_eeprom(int offset) {
  EVENT event;
  byte hifst = EEPROM.read(offset + 0);
  byte lofst = EEPROM.read(offset + 1);
  unsigned int fst = word(hifst, lofst);
  byte hisnd = EEPROM.read(offset + 2);
  byte losnd = EEPROM.read(offset + 3);
  unsigned int snd = word(hisnd, losnd);
  event.t = ((time_t)fst << 16) | snd;
  byte packed_pin_zone = EEPROM.read(offset + 4);
  byte at = packed_pin_zone >> 6;
  event.alarm_type = byte2alarm_type(at);
  event.pin = packed_pin_zone & (B00111111);
  return event;
}

//records a user/engineer password in the EEPROM, depending on choice
void record_password_in_eeprom(int password, byte choice){
  int address = 0;
  if (choice){
    address = USER_PASSWORD_ADDR;   
  }
  else{
    address = ENGINEER_PASSWORD_ADDR;
  }
  byte hipass = highByte(password);
  byte lopass = lowByte(password);
  EEPROM.write(address + 0, hipass);
  EEPROM.write(address + 1, lopass);
}

// reads a saved user/engineer password from the EEPROM
int read_password_from_eeprom(byte choice){
  int address = 0;
  if (choice){
    address = USER_PASSWORD_ADDR;   
  }
  else{
    address = ENGINEER_PASSWORD_ADDR;
  }
  byte hipass = EEPROM.read(address + 0);
  byte lopass = EEPROM.read(address + 1);
  return word(hipass, lopass);
}

// saves all zones settings in the EEPROM
void record_settings_in_eeprom() {
  for(int i = 0; i < NR_ZONES; i++) {
      ZONE zone = zones[i];
      byte at = alarm_type2byte(zone.alarm_type);
      byte hivalue = highByte(zone.value);
      byte lovalue = lowByte(zone.value);
      byte histate = highByte(zone.state);
      byte lostate = lowByte(zone.state);
      
      EEPROM.write(ZONES_ADDR + 5*i + 0, at);
      EEPROM.write(ZONES_ADDR + 5*i + 1, hivalue);
      EEPROM.write(ZONES_ADDR + 5*i + 2, lovalue);
      EEPROM.write(ZONES_ADDR + 5*i + 3, histate);
      EEPROM.write(ZONES_ADDR + 5*i + 4, lostate); 
  }
}

// reads all zone settings from the eeprom
void read_settings_from_eeprom() {
  for(int i = 0; i < NR_ZONES; i++) {
      byte packed_at = EEPROM.read(ZONES_ADDR + 5*i + 0);
      byte hivalue = EEPROM.read(ZONES_ADDR + 5*i + 1);
      byte lovalue = EEPROM.read(ZONES_ADDR + 5*i + 2);
      byte histate = EEPROM.read(ZONES_ADDR + 5*i + 3);
      byte lostate = EEPROM.read(ZONES_ADDR + 5*i + 4);
      
      ALARM_TYPE at = byte2alarm_type(packed_at);
      zones[i].alarm_type = at;
      zones[i].value = word(hivalue, lovalue);
      zones[i].state = word(histate, lostate);
      Serial.print("value:");
      Serial.println(zones[i].value);
      Serial.print("state:");
      Serial.println(zones[i].state);
  }
}

//prints the currently set zone types
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

//checks the zone type and calls the corresponding function
void handle_alarm_zone(ZONE *zone){
  switch (zone->alarm_type) {
    case ENTRY_EXIT:
      handle_entry_exit_zone(zone);
      break;
    case DIGITAL:
      handle_digital_zone(zone);
      break;
    case ANALOG:
      handle_analog_zone(zone);
      break;
    case CONTINUOUS:
      handle_digital_zone(zone);
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
  lcd_print_alarm_types();
}

// timer interrupt called every second
ISR (TIMER1_COMPA_vect) {
  if (show_time)
    lcd_time();
  boolean toggle = false;
  for(byte i = 0; i < NR_ZONES; i++) {
    toggle = toggle || zones[i].triggered || zones[i].active 
      && (zones[i].value + alarm_active_time) > millis()/1000 && zones[i].alarm_type == ENTRY_EXIT;
    if (zones[i].active && zones[i].triggered) {
      if (zones[i].time_triggered + zones[i].value < millis()/1000){
        show_time = false;
        digitalWrite(BUZZER, HIGH);
        record_event_in_eeprom(&zones[i]);
      }
    }
  }
  if (toggle){
    digitalWrite(WARNING_LED, !digitalRead(WARNING_LED));
  }else{
    digitalWrite(WARNING_LED, LOW);
  }
  
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
  pinMode(WARNING_LED, OUTPUT);
  cli(); //disable global interrupts
  TCCR1A = 0;
  TCCR1B = 0;
  OCR1A = 15625; // set the count corresponding to 1 sec
  TCCR1B |= (1 << WGM12); // Turn on CTC mode
  TCCR1B |= (1 << CS10);
  TCCR1B |= (1 << CS12); // prescale at 1024
  TIMSK1 |= (1 << OCIE1A); //enable CTC interrupt
  
  sei(); // enable global interrupts

  //initilize values
  for(int i = 0; i < NR_ZONES; i++){
    zones[i].pin = A0 + i;
    zones[i].value = 0;
    zones[i].state = 0;
  }
  zones[0].alarm_type = ENTRY_EXIT;
  zones[0].value = 10;
  zones[1].alarm_type = DIGITAL;
  zones[2].alarm_type = ANALOG;
  zones[2].value = 128;
  zones[3].alarm_type = CONTINUOUS;
  // Continuous alarm needs the value to be set the value to 0;
  zones[3].value = 0;
  
  //read all cofiguration from the eeprom
  read_settings_from_eeprom();
  user_password = read_password_from_eeprom(0);
  engineer_password = read_password_from_eeprom(1);
  read_alarm_event_offset();

  Serial.begin(9600); 

  // Proof of concept that reading an event from the EEPROM is working.
  // this piece of code reads the last event from the EEPROM and sends it through the serial.
  EVENT eveent = read_event_from_eeprom(alarm_event_offset - EVENT_SIZE);
  Serial.print("time:");
  Serial.print(hour(eveent.t));
  Serial.print(":");
  Serial.print(minute(eveent.t));
  Serial.print(":");
  Serial.print(second(eveent.t));
  Serial.print(":   ");
  Serial.print(day(eveent.t));
  Serial.print(":");
  Serial.print(month(eveent.t));
  Serial.print(":");
  Serial.println(year(eveent.t));
  
  Serial.print("pin");
  Serial.println(eveent.pin - A0);
  Serial.print("at");
  Serial.println(eveent.alarm_type);
  irrecv.enableIRIn(); // Start the receiver
  pinMode(RECV_PIN, INPUT);
}

// gotta love simple functions
void loop() {
  if (irrecv.decode(&results)) {
    CBCaller(keyToInt(results.value));
    irrecv.resume();
  }
  // gotta love for loops
  for (int i = 0; i < NR_ZONES; i++) {
    handle_alarm_zone(&zones[i]);
  }
}

