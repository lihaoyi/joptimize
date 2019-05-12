package test;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface Test {
    int[] inputs() default {};
    String[] checkPresent() default {};
    String[] checkRemoved() default {};
    String[] checkClassRemoved() default {};
    String[] checkMangled() default {};
    String[] checkNotMangled() default {};
    int[] addedNumConst() default {};
    int[] removedNumConst() default {};
    boolean inline() default false;
}