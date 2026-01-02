#pragma once

#include <cstdint>
#include <cstddef>
#include <cstring>
#include <new>

namespace Puma {
namespace Types
{
#pragma pack(push, 1)
    union String
    {
    public:
        // Lifetime
        String() noexcept;
        String(const char* cstr) noexcept;
        String(const String& source) noexcept;
        ~String() noexcept;

        String& operator=(const String& source) noexcept;

        // Public API
		// get str length - length in characters (code points)
		std::uint32_t Length() const noexcept;
		// get str size - length in bytes
        std::uint32_t StrSize() const noexcept;
		// get variable size - variable size in bytes
        std::uint32_t VarSize() const noexcept;

		// get pointer to string data
		const char* Data() const noexcept;

    private:
        // Layout (private)
        struct { std::uint8_t tag; char data[15]; } shortStr;

    #if INTPTR_MAX == INT64_MAX
        struct { std::uint8_t tag; std::uint8_t reserved[3]; std::uint32_t strSize; const char* ptr; } longStr;
    #elif INTPTR_MAX == INT32_MAX
        struct { std::uint8_t tag; std::uint8_t reserved[3]; std::uint32_t strSize; std::uint32_t reserved2; const char* ptr; } longStr;
    #else
    #error Unsupported pointer size
    #endif
		// copy or zero the union
        struct { std::uint64_t firstHalf; std::uint64_t secondHalf; } str;

        // Masks (private) - UPPER_CASE
        static constexpr std::uint8_t SHORT_MASK = 0x80;
        static constexpr std::uint8_t LENGTH_MASK  = 0x0F;
        static constexpr std::uint8_t LONG_MASK  = 0x80;

        // Helpers (private) - lowerCamelCase
        bool isShort() const noexcept;
        bool isLong()  const noexcept;

        void release() noexcept;
        void copyFrom(const String& source) noexcept;
    };
#pragma pack(pop)

} // namespace Types
} // namespace Puma