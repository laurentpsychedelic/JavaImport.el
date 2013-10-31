package my.personal.groovy.project

class TheClass {
    static class SubClass {
        MyGroovyClass1 member = new MyGroovyClass1()
    }
    public static void main(String [] args) {
        new SubClass().member.greet('Laurent')
    }
}

class MyGroovyClass1 {
    def greet(def name) {
        println "Hello, ${name}!"
    }
}