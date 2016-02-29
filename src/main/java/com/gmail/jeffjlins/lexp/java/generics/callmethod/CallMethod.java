package com.gmail.jeffjlins.lexp.java.generics.callmethod;

import java.util.ArrayList;
import java.util.List;
import com.gmail.jeffjlins.lexp.java.generics.methodoverride.Arg;
import com.gmail.jeffjlins.lexp.java.generics.methodoverride.ArgImpl;
import com.gmail.jeffjlins.lexp.java.generics.methodoverride.ArgSuper;
import com.gmail.jeffjlins.lexp.java.generics.methodoverride.Base;

/**
 * Created by jlins on 1/11/16.
 */
public class CallMethod<T extends Arg> {

    public void callIt() {
        Base b = new Base();

//        // does not work
//        b.doThings(new ArgSuper());
        b.doThings(new Arg());
        b.doThings(new ArgImpl());

        b.doThingWithTypeParam(new ArgSuper());
        b.doThingWithTypeParam(new Arg());
        b.doThingWithTypeParam(new ArgImpl());

//        // does not work
//        b.doThingWithExtendTypeParam(new ArgSuper());
        b.doThingWithExtendTypeParam(new Arg());
        b.doThingWithExtendTypeParam(new ArgImpl());

        List<Arg> argList = new ArrayList<>();
        List<ArgImpl> argImplList = new ArrayList<>();
        List<ArgSuper> argSuperList = new ArrayList<>();
        List<? extends Arg> wildcardExtends = new ArrayList<>();
        List<T> typeParamExtends = new ArrayList<>();

        b.doSomethingWithList(argList);
//        // does not work
//        b.doSomethingWithList(argImplList);
//        // does not work
//        b.doSomethingWithList(argSuperList);
//        // does not work
//        b.doSomethingWithList(wildcardExtends);
//        // does not work
//        b.doSomethingWithList(typeParamExtends);

        b.doSomethingWithTypeParamExtendList(argList);
        b.doSomethingWithTypeParamExtendList(argImplList);
//        // does not work
//        b.doSomethingWithTypeParamExtendList(argSuperList);
        b.doSomethingWithTypeParamExtendList(wildcardExtends);
        b.doSomethingWithTypeParamExtendList(typeParamExtends);

        b.doSomethingWithWildcardExtendList(argList);
        b.doSomethingWithWildcardExtendList(argImplList);
//        // does not work
//        b.doSomethingWithWildcardExtendList(argSuperList);
        b.doSomethingWithWildcardExtendList(wildcardExtends);
        b.doSomethingWithWildcardExtendList(typeParamExtends);


    }
}
