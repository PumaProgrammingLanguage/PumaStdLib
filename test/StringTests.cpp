#include "pch.h"
#include "CppUnitTest.h"
#include "../PumaTypes/PumaTypes.hpp"
#include <string>

using Puma::Types::String;

static std::string MakeLong(size_t n)
{
    std::string s;
    s.reserve(n);
    for (size_t i = 0; i < n; ++i) s.push_back(char('a' + (i % 26)));
    return s;
}

TEST_CLASS(StringTests)
{
public:

    TEST_METHOD(ShortInitialize_ReportsSizeAndLength)
    {
        auto s = String::initialize("Hi");

        Assert::AreEqual<uint32_t>(2u, s.StrSize());
        Assert::AreEqual<uint32_t>(2u, s.Length());

        // Finalize with ownership should clear the string without crashing
        s.Finalize(true);
        Assert::AreEqual<uint32_t>(0u, s.StrSize());
        Assert::AreEqual<uint32_t>(0u, s.Length());
    }

    TEST_METHOD(Short15Initialize_ReportsSizeAndLength)
    {
		// longest short string
        auto s = String::initialize("Hello, World!!!");

        Assert::AreEqual<uint32_t>(15u, s.StrSize());
        Assert::AreEqual<uint32_t>(15u, s.Length());

        // Finalize with ownership should clear the string without crashing
        s.Finalize(true);
        Assert::AreEqual<uint32_t>(0u, s.StrSize());
        Assert::AreEqual<uint32_t>(0u, s.Length());
    }

    TEST_METHOD(LongInitialize_ReportsSizeAndLength)
    {
		// shortest long string
        const auto input = MakeLong(16);
        auto s = String::initialize(input.c_str());

        Assert::AreEqual<uint32_t>(16u, s.StrSize());
        Assert::AreEqual<uint32_t>(16u, s.Length());

        // Finalize with ownership should release heap storage and result in empty string
        s.Finalize(true);
        Assert::AreEqual<uint32_t>(0u, s.StrSize());
        Assert::AreEqual<uint32_t>(0u, s.Length());
    }

    TEST_METHOD(Assignment_ShallowCopyBehavior)
    {
        const auto input = MakeLong(40);

		auto a = String::initialize(input.c_str()); // ensure string a is long string
		auto b = String::initialize("short"); // ensure string b is short string

        // before assignment the size/length should not match
        Assert::AreEqual<uint32_t>(40u, a.StrSize());
        Assert::AreEqual<uint32_t>(5u, b.StrSize());
        Assert::AreNotEqual<uint32_t>(a.Length(), b.Length());

        // assignment (API uses operator=(String))
		// string b is short string, so no need to finalize
        b = a;

        // After assignment the size/length should match
        Assert::AreEqual<uint32_t>(40u, a.StrSize());
        Assert::AreEqual<uint32_t>(40u, b.StrSize());
        Assert::AreEqual<uint32_t>(a.Length(), b.Length());

        // if string b is the owneer
		// finalize string a without ownership; For shallow copy, b still refers to the same buffer
        a.Finalize(false);
		Assert::AreEqual<uint32_t>(0u, a.StrSize()); // a cleared
		Assert::AreEqual<uint32_t>(40u, b.StrSize()); // b, no change

        // finalize string b with ownership; 
        b.Finalize(true);
        Assert::AreEqual<uint32_t>(0u, b.StrSize()); // b cleared
		Assert::AreEqual<uint32_t>(0u, a.StrSize()); // a, no change
    }

    TEST_METHOD(InitializeNull_YieldsEmpty)
    {
        auto s = String::initialize(nullptr);
        Assert::AreEqual<uint32_t>(0u, s.StrSize());
        Assert::AreEqual<uint32_t>(0u, s.Length());
        s.Finalize(true); // should be a no-op and not crash
        Assert::AreEqual<uint32_t>(0u, s.StrSize());
        Assert::AreEqual<uint32_t>(0u, s.Length());
    }
};