package edu.jhu.cs.bigbang.eclipse.editors;

import java.util.ArrayList;
import java.util.List;

public class Person {

	private String name;
	
	public Person(String name) {
		this.name = name;
	}
	
	public String getName() {
		return name;
	}
	
	public void setName(String name) {
		this.name = name;
	}
	
	public static void main(String[] args) {
		
		// Old list contains A, B, C
		List<Person> oldList = new ArrayList<Person>();
		oldList.add(new Person("A"));
		oldList.add(new Person("B"));
		oldList.add(new Person("C"));
		
		// New list, has everything from the old list
		List<Person> newList = new ArrayList<Person>();
		for(Person p : oldList)
			newList.add(p);
		
		// Change everything in the new list
		for(Person p : newList)
			p.setName("Hack " + p.getName());
		
		// See what happens to the old list
		for(Person p : oldList)
			System.out.println(p.getName());
		
	}
	
}
